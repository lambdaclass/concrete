import Concrete.Proof.Digest
/-
  Kernel replay as a typed service (R-0004 package 3).

  "Did the Lean kernel accept this?" is the question every replay-backed receipt rests on, and until
  now it had no answerer — only a rendering. The check lived INLINE in `compileAndReport`, tangled
  with that function's `reportJson` parameter, and its failure paths PRINTED and returned an exit
  code. Nothing else could ask the question: not `concrete check`, not a report, not the link
  migration, not a test. A second caller would have had to re-implement it, which is how two answers
  to one question get born.

  So replay is extracted here as ONE producer:

      ReplayRequest -> IO (Except ReplayRefusal ReplayResult)

  with the properties that make it usable as evidence rather than as output:

  * TYPED INPUTS. No JSON flag, no rendering parameter, no exit code. The request says what to check
    and where; it does not say how to say it.
  * EVERY FAILURE CLASS IS RETURNED. A missing workspace, an unwritable scratch file, an absent
    checker are values, not printed lines. A caller that must fail closed can pattern-match; a caller
    that renders can render. Neither can silently skip one.
  * THE ENVIRONMENT IS RETAINED. Workspace, toolchain and import closure travel with the result,
    because a verdict that cannot say which checker produced it cannot be audited later — and a
    receipt minted from it would be recording an anonymous claim.
  * NO SIDE EFFECTS ON EVIDENCE. This function mints nothing and writes no receipt. Minting is a
    separate authority that CONSUMES a successful result; keeping them apart is what makes
    "unchecked facts -> receipt" unconstructible.

  WHAT IT DELIBERATELY DOES NOT DO: decide policy. Whether an unbound-but-type-checking theorem is
  acceptable, whether a rejection should fail the build, what exit code any of it maps to — all of
  that belongs to the caller. This function reports what the kernel did.
-/

namespace Concrete.Proof

/-- Which kind of obligation a replay target discharges. -/
inductive ReplayKind where
  | refinement   -- a `#[proof_by]` link from a claim to a Lean theorem
  | ensures      -- an `#[ensures_proof]` discharge of a source contract
  deriving BEq, Repr, DecidableEq

def ReplayKind.canonical : ReplayKind → String
  | .refinement => "refinement"
  | .ensures    => "ensures"

/-- How the link being replayed was established. -/
inductive ReplayOrigin where
  | sourceLinked  -- synthesized from an in-source proof link
  | hardcoded     -- from the compiler's own `provedFunctions` table
  deriving BEq, Repr, DecidableEq

def ReplayOrigin.canonical : ReplayOrigin → String
  | .sourceLinked => "source_linked"
  | .hardcoded    => "hardcoded"

/-- Whether the claim has a stored proof-subject digest.

    This is NOT a detail of rendering. A theorem can type-check perfectly while the claim it is
    attached to has no stored subject, in which case the freshness comparison has nothing to compare
    against and a changed body would go undetected. Such a target is `unbound`: the kernel accepted
    it, and it is still not proved. Carrying the distinction in the TYPE is what stops a caller from
    adding the two counts together — which is exactly what happened on `examples/hmac_sha256`, where
    a summary read "11 verified" while ProofCore was concurrently emitting 11 "this claim is unbound,
    not proved" errors.

    Modelled as an inductive rather than a `Bool` on purpose: a positional boolean in a record of
    strings is the shape that lets an argument slide into the wrong slot. -/
inductive ReplayBinding where
  | bound     -- a stored proof-subject digest exists, so freshness is decidable
  | unbound   -- no stored subject; acceptance means type-checked, not proved
  deriving BEq, Repr, DecidableEq

/-- One theorem the kernel is being asked to accept, with everything needed to identify it
    afterwards. `subject` is the claim; `theoremName` is the artifact. -/
structure ReplayTarget where
  subject     : String   -- qualified name of the function whose claim this discharges
  theoremName : String   -- fully-qualified Lean theorem — the artifact identity
  kind        : ReplayKind
  origin      : ReplayOrigin
  binding     : ReplayBinding
  deriving BEq, Repr

/-- What to replay, and where.

    `inputPath` and `fallbackDir` are the two workspace candidates, in that order (see
    `resolveWorkspace`). `imports` is the closure the generated file declares — an input rather than
    a constant, so a migration or test can replay against a narrower environment without a second
    copy of this producer. -/
structure ReplayRequest where
  inputPath   : String
  fallbackDir : String := "."
  imports     : List String
  targets     : List ReplayTarget
  deriving Repr

/-- Everything that can stop a replay from producing a verdict.

    Each of these was previously an `IO.println` followed by `return 1`, which meant a caller could
    not distinguish "the kernel rejected a theorem" from "no kernel ever ran". Those are opposite
    facts: the first is evidence, the second is the absence of it, and a receipt minted without the
    distinction would record a refusal as a pass. -/
inductive ReplayRefusal where
  /-- No Lake workspace was found from the input or the fallback directory. The theorems were never
      looked at; blaming them would send the reader to the wrong place. -/
  | noWorkspace (inputPath : String) (fallbackDir : String)
  /-- The request named nothing to check. Deliberately a REFUSAL rather than an empty success: a
      vacuous "all accepted" is exactly the answer a minting path must never receive, because
      `checks.all accepted` is `true` over an empty list. Callers that legitimately have nothing to
      do (the CLI, when a program carries no proof links) match this case and say so. -/
  | noTargets
  /-- A target carried an empty subject or theorem name. Such a target cannot be searched for in the
      transcript — the empty needle matches everywhere — so it would silently report as accepted. -/
  | unnamedTarget (subject : String) (theoremName : String)
  /-- The scratch file could not be created or written. -/
  | scratchUnavailable (detail : String)
  /-- The checker could not be invoked at all (no `lake` on PATH, permission denied). Distinct from
      a non-zero exit: this is "no kernel ran", not "the kernel said no". -/
  | checkerUnavailable (detail : String)
  deriving BEq, Repr

/-- A one-line explanation naming what is wrong, for callers that render. The message names the
    OBSTACLE rather than the targets, because reporting N missing theorems for one missing workspace
    is what sent readers looking for the wrong thing. -/
def ReplayRefusal.explain : ReplayRefusal → String
  | .noWorkspace inp fb =>
      s!"cannot locate a Lake workspace for '{inp}' (no lakefile.toml or lakefile.lean in any parent "
      ++ s!"directory of it or of '{fb}'), so the Lean theorems it references cannot be replayed"
  | .noTargets =>
      "no proved, stale, or unbound obligations with proof names to check"
  | .unnamedTarget s t =>
      s!"replay target has an empty name (subject='{s}', theorem='{t}'); it cannot be located in the "
      ++ "checker transcript and would report as accepted without being checked"
  | .scratchUnavailable d => s!"could not write the generated replay file: {d}"
  | .checkerUnavailable d => s!"could not invoke the Lean checker: {d}"

/-- A short stable tag per refusal, for structured surfaces that must not parse prose. -/
def ReplayRefusal.canonical : ReplayRefusal → String
  | .noWorkspace ..       => "no_workspace"
  | .noTargets            => "no_targets"
  | .unnamedTarget ..     => "unnamed_target"
  | .scratchUnavailable _ => "scratch_unavailable"
  | .checkerUnavailable _ => "checker_unavailable"

/-- What the kernel did with one target. -/
inductive ReplayVerdict where
  /-- The kernel accepted it AND the claim has a stored subject. This is the only verdict that may
      support a receipt. -/
  | accepted
  /-- The kernel accepted it, but the claim is unbound: type-checked, NOT proved. Separated from
      `accepted` so no caller can sum them into a coverage number. -/
  | acceptedUnbound
  /-- The kernel reported an error mentioning this theorem. -/
  | rejected
  /-- The run failed as a whole, so this target has no individual verdict. NOT the same as
      `rejected`: nothing is known about this theorem, and saying it failed would blame an artifact
      that was very likely fine. -/
  | notAttempted
  deriving BEq, Repr, DecidableEq

def ReplayVerdict.canonical : ReplayVerdict → String
  | .accepted        => "accepted"
  | .acceptedUnbound => "accepted_unbound"
  | .rejected        => "rejected"
  | .notAttempted    => "not_attempted"

/-- A verdict paired with the target it belongs to. -/
structure ReplayCheck where
  target  : ReplayTarget
  verdict : ReplayVerdict
  deriving BEq, Repr

/-- The Lean checker that actually ran, identified by the process itself and by its build.

    BEFORE R-0208 the checker was named by the TEXT of the workspace's `lean-toolchain` file, which
    is not the process that replays. The nix devshell takes Lean from nixpkgs and ignores that file
    while CI's elan honours it, so two different checkers carried the same name, and an upgraded
    kernel could not be told from the one it replaced.

    `version`, `githash` and `platform` are what the checking process reports about itself.
    `buildDigest` is SHA-256 over the bytes of the executable that process ran from AND the
    toolchain's shared libraries beside it: the `lean` executable is a small stub, and the kernel
    lives in `libleanshared`, so digesting the executable alone would bind almost nothing. A nix
    build reports its githash as the version tag, which is exactly why the bytes are bound too.

    WHAT IT DOES NOT BIND: system libraries outside the toolchain prefix (libc, GMP, libuv), the
    `.olean` files of Lean's own core library, and anything about HOW the identity reached us. The
    replay process prints it, tagged with a per-run nonce, which CORRELATES the line with that run;
    it does not AUTHENTICATE it — code running inside the checker could print the same line. -/
structure CheckerIdentity where
  version     : String
  githash     : String
  platform    : String
  buildDigest : String
  deriving BEq, Repr

/-- The one rendering a receipt's toolchain identity is computed from. -/
def CheckerIdentity.canonical (c : CheckerIdentity) : String :=
  s!"lean {c.version} commit {c.githash} platform {c.platform} build {c.buildDigest}"

/-- Who checked, or WHY that could not be established.

    An unidentified checker used to be a bare `none`, and every report said only "UNIDENTIFIED". On
    2026-10-23 that cost hours: a gate host whose non-login PATH lacked `shasum` withheld every
    receipt, and nothing in the output pointed at the missing tool. The cause is now part of the
    value, produced where the failure happens, so every report and refusal can name it. It is never
    a wildcard: `unidentified` mints nothing and is current against nothing, whatever its reason. -/
inductive CheckerResolution where
  | identified (checker : CheckerIdentity)
  | unidentified (why : String)
  deriving BEq, Repr

def CheckerResolution.identity? : CheckerResolution → Option CheckerIdentity
  | .identified c => some c
  | .unidentified _ => none

/-- The identity's canonical form, or `UNIDENTIFIED (<why>)`. -/
def CheckerResolution.display : CheckerResolution → String
  | .identified c => c.canonical
  | .unidentified why => s!"UNIDENTIFIED ({why})"

/-- The checker that produced a result, identified well enough to be re-examined later.

    A verdict with no environment is anonymous, and an anonymous verdict cannot be invalidated: when
    the toolchain moves or the import closure changes, nothing knows the old answer is stale. Slice 4
    found this the hard way — the same file gave "3 verified" from one directory and "0 verified, 3
    failed" from another, and the report said neither where it had looked nor with what. -/
structure ReplayEnvironment where
  workspace         : String
  /-- `true` when the workspace came from the input, `false` when it came from the fallback
      directory. The distinction is the difference between a location-independent verdict and one
      that depended on where the caller happened to stand. -/
  workspaceFromInput : Bool
  toolchain         : String
  /-- The libraries the request ASKED to import, by name. -/
  imports           : List String
  /-- The libraries the replay actually found, by CONTENT: one digest per Lean source file, sorted
      by module path.

      THIS IS THE PROOF LIBRARY'S IDENTITY, and its absence was an authority gap rather than a
      documentation one. `importsId` is meant to bind the transitive import surface; before
      2026-08-16 the receipt bound a digest of the compiler's own checked-in classification table,
      which is compiled into the binary — so replaying the same theorem names against a DIFFERENT
      proof library produced a byte-identical receipt. Nothing in the evidence identified whose
      theorems had been accepted.

      Digested from SOURCE rather than from `.olean` files on purpose: object files are not
      guaranteed byte-reproducible across machines, and a receipt whose currency depended on that
      would fail clean-checkout reproducibility for reasons unrelated to the program. Source is
      reproducible by construction, and it is what the theorems actually are.

      WHAT IT STILL DOES NOT BIND, stated because a receipt that overclaims is worse than one that
      binds less: anything outside the workspace. Lean core is pinned by `toolchain`; a proof
      reaching some other external library is not covered here. -/
  importDigests     : List (String × String)
  /-- The checker that ran, as it reported itself, or why no identity could be established.
      `unidentified` mints nothing and compares current against nothing — it is never a wildcard.
      Defaulted only so a hand-built environment fails for the reason it should (private
      constructors downstream), not for a missing field. -/
  checker           : CheckerResolution := .unidentified "no checker was asked who it is"
  deriving BEq, Repr

/-- What a FRESH KERNEL RE-CHECK said about one theorem (R-0208 decision 1).

    `#check @thm` only looks a name up in `.olean` files an earlier build produced; it accepted an
    ill-typed theorem added with the kernel check skipped (measured 2026-10-11). The re-check replays
    the declarations of every workspace module in the theorem's import closure through the kernel,
    in the same process whose identity the result records. Modules shipped with the toolchain are
    not replayed: they are part of the checker build that `CheckerIdentity` names. -/
inductive RecheckVerdict where
  /-- Every workspace module in the closure replayed through the kernel without error. -/
  | rechecked
  /-- The theorem rests on native evaluation (a `._native.` axiom; up to Lean 4.34 also
      `Lean.ofReduceBool`/`ofReduceNat`/`trustCompiler`; see `nativeAxiomClassifierSrc`). The kernel
      cannot re-run compiled code, so no replay can re-establish it; never treated as rechecked. -/
  | native
  /-- Some module in the closure failed to replay; the message names it. -/
  | failed (why : String)
  /-- The run did not report exactly one re-check verdict for this theorem. -/
  | notRun
  deriving BEq, Repr

def RecheckVerdict.canonical : RecheckVerdict → String
  | .rechecked => "rechecked"
  | .native    => "native_trust"
  | .failed _  => "recheck_failed"
  | .notRun    => "recheck_not_run"

/-- What a completed replay produced. It records what happened, INCLUDING failure — a report that
    refused to describe a failed run would just hide failures.

    The constructor is PRIVATE, and that is the load-bearing part. `replay` is the only producer, so
    a `ReplayResult` is a claim that a kernel actually ran. Without this, anything could assemble a
    result whose every verdict said `accepted` and hand it to the minting authority, which is the
    forged-replay bypass the whole chain exists to close: the type would be documenting a hole rather
    than preventing one. Projections stay public; only construction is closed. -/
structure ReplayResult where
  private mk ::
  environment    : ReplayEnvironment
  checks         : List ReplayCheck
  exitCode       : UInt32
  /-- The checker exited non-zero without naming any individual theorem: the file did not compile as
      a whole. Every target is `notAttempted`, and the transcript is the only diagnosis available.

      Computed BEFORE the per-target fallback that marks targets, which is where the previous inline
      version went wrong: it derived this flag from `failed.isEmpty` AFTER filling `failed` with
      every target, so the flag was unreachably false and a whole-file compile error was reported as
      every theorem individually "not found". -/
  generalFailure : Bool
  /-- Combined stdout and stderr, retained verbatim. The only diagnosis available for a general
      failure, and the material every per-target verdict was read out of. -/
  transcript     : String
  /-- The fresh kernel re-check, one verdict per distinct theorem name. -/
  recheck        : List (String × RecheckVerdict) := []
  deriving BEq, Repr

namespace ReplayResult

def accepted (r : ReplayResult) : List ReplayCheck :=
  r.checks.filter (·.verdict == .accepted)

def acceptedUnbound (r : ReplayResult) : List ReplayCheck :=
  r.checks.filter (·.verdict == .acceptedUnbound)

def rejected (r : ReplayResult) : List ReplayCheck :=
  r.checks.filter (·.verdict == .rejected)

def notAttempted (r : ReplayResult) : List ReplayCheck :=
  r.checks.filter (·.verdict == .notAttempted)

/-- Did every target reach a verdict the kernel is happy with?

    `acceptedUnbound` counts as accepted HERE because the kernel genuinely accepted the theorem —
    this predicate answers "did the checker complain", not "is this proved". Receipt minting must not
    use it for the second question; that is what `fullyBound` is for.

    The `!generalFailure` conjunct is REDUNDANT today and kept deliberately: a general failure
    assigns every target `notAttempted`, which already fails the test below. It is defence against a
    future change in verdict assignment, and because it is redundant it cannot be mutation-killed —
    recorded here so its survival is not read as an untested boundary. -/
def allAccepted (r : ReplayResult) : Bool :=
  !r.generalFailure
    && r.checks.all (fun c => c.verdict == .accepted || c.verdict == .acceptedUnbound)

/-- Every target was accepted AND bound — the only shape from which proof-backed receipts may be
    minted for the whole request. Vacuously true on an empty check list, which is why an empty
    request is refused rather than returned as a result. -/
def fullyBound (r : ReplayResult) : Bool :=
  !r.generalFailure && r.checks.all (·.verdict == .accepted)

/-- The verdict for one theorem, by artifact name. -/
def verdictFor (r : ReplayResult) (theoremName : String) : Option ReplayVerdict :=
  (r.checks.find? (·.target.theoremName == theoremName)).map (·.verdict)

end ReplayResult

/-! ### The minting token

Receipts may be minted from a successful replay and from nothing else. That rule is enforced by
making the WITNESS of a successful replay a type that cannot be assembled — not by a predicate a
caller is asked to check first, because a predicate can be forgotten and a forgotten predicate is
indistinguishable from a satisfied one. -/

/-- Everything that stops a completed replay from witnessing one theorem's acceptance. -/
inductive ReplayEvidenceRefusal where
  /-- The theorem was not in the request, so this run says nothing about it. Distinct from a
      rejection: silence is not a verdict. -/
  | notReplayed (theoremName : String)
  /-- The kernel reached a verdict, and it was not `accepted`. Carries which one, because
      `acceptedUnbound` and `rejected` are refused for opposite reasons and a caller repairing the
      cause needs to know which. -/
  | notAccepted (theoremName : String) (verdict : ReplayVerdict)
  /-- The file did not compile, so no verdict in this run means anything. -/
  | underGeneralFailure
  /-- The workspace was resolved from the caller's directory rather than from the input. A receipt is
      durable evidence that must be re-checkable from the artifact alone, and a verdict that depended
      on where the caller happened to stand is not a property of the program — it is exactly the
      location-dependence slice 4 removed. Replaying such an input is still legitimate and still
      reported; what it may not do is mint. -/
  | fallbackWorkspace (workspace : String)
  /-- The run did not establish which checker produced its verdicts, for the reason given. A
      receipt binds the checker, and binding an unknown one would let any later checker match it. -/
  | checkerUnidentified (why : String)
  /-- The theorem was found but not RE-CHECKED by the kernel in this run (decision 1): a lookup in
      an `.olean` is not a check. -/
  | notRechecked (theoremName : String) (verdict : RecheckVerdict)
  deriving BEq, Repr

def ReplayEvidenceRefusal.canonical : ReplayEvidenceRefusal → String
  | .notReplayed _       => "not_replayed"
  | .notAccepted ..      => "not_accepted"
  | .underGeneralFailure => "under_general_failure"
  | .fallbackWorkspace _ => "fallback_workspace"
  | .checkerUnidentified _ => "checker_unidentified"
  | .notRechecked ..     => "not_rechecked"

def ReplayEvidenceRefusal.explain : ReplayEvidenceRefusal → String
  | .notReplayed t =>
      s!"'{t}' was not part of this replay, so the run says nothing about it"
  | .notAccepted t v =>
      s!"the kernel's verdict for '{t}' was '{v.canonical}', not 'accepted'"
  | .underGeneralFailure =>
      "the replayed file did not compile, so no verdict in this run witnesses anything"
  | .fallbackWorkspace w =>
      s!"the workspace '{w}' was resolved from the caller's directory rather than from the input, so "
      ++ "the verdict is not a property of the program alone and may not be minted into a receipt"
  | .checkerUnidentified why =>
      "the run did not establish which checker executable and build produced its verdicts "
      ++ s!"({why}), so there is nothing a receipt could bind"
  | .notRechecked t v =>
      s!"'{t}' was found but not re-checked by the kernel in this run ({v.canonical}"
      ++ (match v with | .failed why => s!": {why}" | _ => "")
      ++ "); a lookup in a compiled .olean is not a kernel check"

/-- The kernel accepted THIS theorem, in THIS environment.

    PRIVATE CONSTRUCTOR, and no `Inhabited`: `of?` is the only way to obtain one, and it requires a
    `ReplayResult` — which `replay` is the only producer of. That is the whole chain. `unchecked
    facts -> receipt` does not merely refuse at runtime; it does not typecheck, because there is no
    way to name the token without having run a kernel.

    `acceptedUnbound` is deliberately NOT enough. The kernel genuinely accepted the theorem, but the
    claim has no stored proof-subject digest, so a receipt minted from it would record freshness
    against nothing. -/
structure SuccessfulReplay where
  private mk ::
  theoremName : String
  subject     : String
  environment : ReplayEnvironment
  /-- The checker that accepted the theorem. Total here: `of?` refuses a run without one. -/
  checker     : CheckerIdentity
  deriving Repr

/-- Extract the witness for one theorem from a completed replay, or say why not. -/
def SuccessfulReplay.of? (r : ReplayResult) (theoremName : String)
    : Except ReplayEvidenceRefusal SuccessfulReplay := do
  if r.generalFailure then throw .underGeneralFailure
  let some c := r.checks.find? (·.target.theoremName == theoremName)
    | throw (.notReplayed theoremName)
  if c.verdict != .accepted then throw (.notAccepted theoremName c.verdict)
  if !r.environment.workspaceFromInput then
    throw (.fallbackWorkspace r.environment.workspace)
  let checker ← match r.environment.checker with
    | .identified c => pure c
    | .unidentified why => throw (.checkerUnidentified why)
  let rv := ((r.recheck.find? (·.1 == theoremName)).map (·.2)).getD .notRun
  if rv != .rechecked then throw (.notRechecked theoremName rv)
  return { theoremName := c.target.theoremName
         , subject := c.target.subject
         , environment := r.environment
         , checker }

/-! ### The producer -/

/-- Walk up from `path` for the directory holding a Lake workspace.

    Returns `none` rather than guessing: a wrong workspace replays against the wrong library and
    reports confident nonsense. A path that does not exist is `none` too, not an exception — the
    caller's refusal for "no workspace" is the right report for it. -/
partial def findLakeWorkspace (path : String) : IO (Option System.FilePath) := do
  try
    let abs ← IO.FS.realPath (System.FilePath.mk path)
    let rec up (dir : System.FilePath) (fuel : Nat) : IO (Option System.FilePath) := do
      match fuel with
      | 0 => return none
      | fuel + 1 =>
        if (← (dir / "lakefile.toml").pathExists) || (← (dir / "lakefile.lean").pathExists) then
          return some dir
        match dir.parent with
        | some p => if p == dir then return none else up p fuel
        | none => return none
    let start := if (← abs.isDir) then abs else (abs.parent.getD abs)
    up start 64
  catch _ => return none

/-- Resolve the workspace from the INPUT first, then the caller's directory.

    Resolving ONLY from the input was tried and was too strict: a tool that copies sources to a temp
    directory and replays them from inside the repo (which is how `check_purecore_proofs.sh`
    exercises std) gives an input with no workspace of its own, and refusing it reported 12 real,
    kernel-verified proofs as unreachable. The cwd-independence property is preserved where it means
    something — an input that HAS a workspace always resolves to that one, so the same file gives the
    same verdict from anywhere. Only an input with none falls back, and the result records which case
    applied so the answer is never anonymous. -/
def resolveWorkspace (req : ReplayRequest) : IO (Option (System.FilePath × Bool)) := do
  match ← findLakeWorkspace req.inputPath with
  | some w => return some (w, true)
  | none   => return (← findLakeWorkspace req.fallbackDir).map (·, false)

/-- The content digest of a proof library inside a workspace: one entry per Lean source file,
    keyed by workspace-relative path and sorted, so the value is a property of the LIBRARY and not of
    where the checkout sits.

    An import name maps to `<ws>/<Name>.lean` (the library root) plus `<ws>/<Name>/**.lean`. A name
    that resolves to nothing contributes nothing — and an EMPTY closure is what the minting path
    refuses on, since a receipt binding no library identifies no library. -/
def importClosureOf (ws : System.FilePath) (imports : List String)
    : IO (List (String × String)) := do
  let mut out : List (String × String) := []
  for lib in imports do
    let root := ws / (lib ++ ".lean")
    let dir := ws / lib
    let mut files : List System.FilePath := []
    if ← root.pathExists then files := files ++ [root]
    if ← dir.isDir then
      let walked ← System.FilePath.walkDir dir
      files := files ++ (walked.toList.filter fun f => f.toString.endsWith ".lean")
    for f in files do
      let content ← try IO.FS.readFile f catch _ => pure ""
      -- Keyed by the path RELATIVE to the workspace: an absolute path would make the digest a
      -- property of the checkout, which is the location-dependence this exists to avoid.
      let pfx := ws.toString ++ "/"
      let fs := f.toString
      let rel := if fs.startsWith pfx then String.ofList (fs.toList.drop pfx.length) else fs
      out := out ++ [(rel, Concrete.shortHash content)]
  return out.mergeSort (fun a b => a.1 ≤ b.1)

/-- The Lean command that makes the process running it report its own identity. Appended to every
    replay file, so the identity in a result is the identity of the process that produced its
    verdicts — not of a second process asked afterwards, which could resolve differently.

    It prints one `CONCRETE-CHECKER-ID` line (version, githash, platform, executable) and one
    `CONCRETE-CHECKER-IMAGE` line per file the process has MAPPED — read from `/proc/self/maps` on
    Linux and from `lsof` on the process itself elsewhere. That is the code that actually loaded,
    which is the point: hashing the libraries that merely sit beside the executable binds a file the
    process may never have opened (macOS loads four of the toolchain's five dylibs) and misses ones
    it did (GMP and libuv come from elsewhere). Classification of the raw list is done by the
    consumer, in `checkerImagesOf`, where it can be tested without a checker. -/
def checkerIdentityProbe (nonce : String) : String :=
  (r#"
-- The checker reports itself; the nonce ties these lines to this run.
#eval show IO Unit from do
  IO.println s!"CONCRETE-CHECKER-ID NONCE {Lean.versionString}|{Lean.githash}|{System.Platform.target}|{← IO.appPath}"
  let maps : System.FilePath := "/proc/self/maps"
  let paths ← if ← maps.pathExists then do
      let s ← IO.FS.readFile maps
      pure (s.splitOn "\n" |>.filterMap fun l =>
        match l.splitOn " /" with
        | _ :: rest@(_ :: _) => some ("/" ++ " /".intercalate rest)
        | _ => none)
    else do
      let o ← IO.Process.output
        { cmd := "lsof", args := #["-p", toString (← IO.Process.getPID), "-Fn"] }
      if o.exitCode != 0 then pure [] else
      pure (o.stdout.splitOn "\n" |>.filterMap fun l =>
        if l.startsWith "n/" then some (String.ofList (l.toList.drop 1)) else none)
  -- NATIVE CODE ONLY, filtered here: a replay importing the proof library maps ~15 000 `.olean`
  -- files, and listing (let alone de-duplicating) them in interpreted Lean cost seconds per run.
  -- The consumer still classifies what is printed; duplicates are its job too.
  let app := (← IO.appPath).toString
  for p in paths do
    if p == app || p.endsWith ".dylib" || p.endsWith ".dll" || p.endsWith ".so"
       || (p.splitOn ".so.").length > 1 then
      IO.println s!"CONCRETE-CHECKER-IMAGE NONCE {p}"
"#).replace "NONCE" nonce

/-- A fresh per-run nonce. -/
def freshNonce : IO String := do
  let b ← IO.getRandomBytes 16
  return String.join (b.toList.map byteToHex)

/-- The payloads of the lines tagged `tag` and carrying `nonce`. -/
def taggedLines (tag nonce transcript : String) : List String :=
  let pfx := s!"{tag} {nonce} "
  (transcript.splitOn "\n").filterMap fun l =>
    if l.startsWith pfx then some (String.ofList (l.toList.drop pfx.length)).trimAscii.toString
    else none

/-- The fields of the ONE identity line carrying `nonce`, or `none`. Two lines with the right nonce
    are as bad as none: the run did not say unambiguously who it was. -/
def parseCheckerLine (nonce transcript : String) : Option (String × String × String × String) :=
  match taggedLines "CONCRETE-CHECKER-ID" nonce transcript with
  | [l] =>
    match l.splitOn "|" with
    | [v, g, p, a] => if [v, g, p, a].any (·.isEmpty) then none else some (v, g, p, a)
    | _ => none
  | _ => none

/-- OS-owned locations. Images here are NAMED in the identity but not hashed: binding the host's
    libc bytes would make every receipt flap with routine OS updates while saying nothing about the
    Lean build. A stated limit, not a claim that they cannot matter. -/
def isSystemImage (p : String) : Bool :=
  ["/usr/lib/", "/lib/", "/lib64/", "/usr/lib64/", "/System/"].any (fun (d : String) => p.startsWith d)

/-- A file of native code: a shared library by name, or the executable itself. Data files the
    process maps (`.olean`, `.ir`) are not code and are not part of WHICH checker this is. -/
def isNativeImage (exe p : String) : Bool :=
  let name := (System.FilePath.mk p).fileName.getD ""
  p == exe || name.endsWith ".dylib" || name.endsWith ".dll" || name.endsWith ".so"
    || (name.splitOn ".so.").length > 1

/-- Which loaded images the identity hashes and which it only names, or why there is no identity.

    Refuses rather than guessing when: the process listed no images; its own executable is not among
    them; no `libleanshared` was loaded (the kernel's code was not located); or two hashed images
    share a file name (keys are file names, so the digest is independent of install location, and an
    ambiguous key would let one stand in for the other). -/
def checkerImagesOf (exe : String) (images : List String)
    : Except String (List String × List String) := do
  let native := images.eraseDups.filter (isNativeImage exe)
  if native.isEmpty then throw "the checker listed no loaded images"
  if !native.contains exe then throw s!"the checker's executable '{exe}' is not among its loaded images"
  let nameOf := fun (p : String) => (System.FilePath.mk p).fileName.getD p
  if !native.any (fun p => (nameOf p).startsWith "libleanshared") then
    throw "no libleanshared was loaded, so the kernel's code was not located"
  let hashed := native.filter (!isSystemImage ·)
  let names := hashed.map nameOf
  if names.eraseDups.length != names.length then
    throw "two loaded images share a file name, so a name cannot identify one of them"
  return (hashed, native.filter isSystemImage)

/-- SHA-256 of each file, in order, by the host's `shasum -a 256`, or why not.

    An external tool on purpose. These are ~200 MB of native code, and `Sha256Fast` evaluated by
    the Lean interpreter — which is how every `#eval`-level caller of `replay` runs it — does not
    finish; even compiled it cost ~2.5 s per identity. `shasum` ships with macOS and with Perl on
    Linux runners; it joins `lsof` in the trust base of the identity, and a host without it has no
    identity rather than a weaker one — and the reason names the tool, so that host can be fixed. -/
def sha256Files (paths : List String) : IO (Except String (List String)) := do
  if paths.isEmpty then return .ok []
  let out ← try
      pure (Except.ok (← IO.Process.output { cmd := "shasum", args := #["-a", "256"] ++ paths.toArray }))
    catch e => pure (Except.error s!"shasum unavailable: {e}")
  let o ← match out with
    | .ok o => pure o
    | .error why => return .error why
  if o.exitCode != 0 then
    let err := ((o.stderr.splitOn "\n").find? (!·.trimAscii.isEmpty)).getD ""
    return .error s!"shasum unavailable: `shasum -a 256` exited {o.exitCode}{if err.isEmpty then "" else s!": {err.trimAscii}"}"
  let rows := (o.stdout.splitOn "\n").filter (!·.isEmpty)
  let isHex := fun (c : Char) => c.isDigit || ('a' ≤ c && c ≤ 'f')
  let digests := rows.map fun r => String.ofList (r.toList.take 64)
  if rows.length == paths.length && digests.all (fun d => d.length == 64 && d.all isHex) then
    return .ok digests
  return .error s!"shasum printed {rows.length} line(s) for {paths.length} image(s), not one SHA-256 each"

/-- SHA-256 over the hashed images' digests (keyed by file name) and the named images' paths. -/
def checkerBuildDigest (hashed named : List String) : IO (Except String String) := do
  let digests ← match ← sha256Files hashed with
    | .ok d => pure d
    | .error why => return .error why
  let nameOf := fun (p : String) => (System.FilePath.mk p).fileName.getD p
  let rows := (hashed.zip digests).map (fun (p, d) => s!"hash {nameOf p}:{d}") ++ named.map (s!"name {·}")
  let material := "\n".intercalate (rows.mergeSort (· ≤ ·))
  return .ok (String.join ((Sha256Fast.hash material.toUTF8).toList.map byteToHex))

/-- The identity a run reported about itself, completed with the digest of the code it had loaded,
    or the first reason it could not be. -/
def checkerIdentityOf (nonce transcript : String) : IO CheckerResolution := do
  let some (version, githash, platform, app) := parseCheckerLine nonce transcript
    | let n := (taggedLines "CONCRETE-CHECKER-ID" nonce transcript).length
      return .unidentified (if n == 0 then "the checker printed no identity line for this run"
        else s!"the checker printed {n} identity lines for this run, not exactly one well-formed line")
  match checkerImagesOf app (taggedLines "CONCRETE-CHECKER-IMAGE" nonce transcript) with
  | .error why => return .unidentified why
  | .ok (hashed, named) =>
    match ← checkerBuildDigest hashed named with
    | .error why => return .unidentified why
    | .ok buildDigest => return .identified { version, githash, platform, buildDigest }

/-- WHICH AXIOMS ARE NATIVE EVALUATION, as Lean SOURCE: spliced at the head of `recheckProbe`, and
    compiled on its own, under every supported toolchain, by `check_kernel_recheck.sh`. One text, so
    the rule the replay applies is the rule the gate tests.

    Two spellings of native trust exist. Lean up to 4.34 declares `Lean.ofReduceBool`,
    `Lean.ofReduceNat` and `Lean.trustCompiler`; Lean 4.35 removes them (lean4#14953), and native
    evaluation is then only the axiom `Lean.Meta.nativeEqTrue` mints per use,
    `<decl>._native.<tactic>.ax_<n>` (`bv_decide`, `native_decide`, `decide +native`), which 4.34
    already uses. The legacy names are SINGLE-backtick literals: a double-backtick literal must
    resolve, so it would stop the probe compiling on 4.35 and every theorem would read `notRun`.
    They stay in the list because a 4.34 environment can still rest on them; on 4.35 they match
    nothing, which the gate checks against the toolchain's own axiom set. -/
def nativeAxiomClassifierSrc : String := r#"
/-- The native-evaluation axioms Lean declared up to 4.34 (removed in 4.35, lean4#14953). -/
def _concreteLegacyNativeAxioms : List Lean.Name :=
  [`Lean.ofReduceBool, `Lean.ofReduceNat, `Lean.trustCompiler]

/-- An axiom that stands for natively evaluated code: a legacy one, or one minted by
    `Lean.Meta.nativeEqTrue` (a `_native` name component). -/
def _concreteIsNativeAxiom (a : Lean.Name) : Bool :=
  _concreteLegacyNativeAxioms.contains a || (a.toString.splitOn "._native.").length > 1
"#

/-- The Lean commands that RE-CHECK each theorem through the kernel, in the replay process itself.

    For each theorem: if it rests on native evaluation it is reported `native`; otherwise every
    WORKSPACE CONSTANT it reaches (through types, values and recursor rules, plus each inductive's
    constructors and recursors) is replayed with `Lean.Kernel.Environment.replay` — the routine
    `leanchecker` uses (merged into Lean core by 4.34; 4.28 shipped it as `LeanChecker.Replay`'s `replay'`): declarations go to the kernel, constructors and recursors are compared with
    what the kernel regenerates — into an environment holding only the toolchain modules those
    constants touch. Constants from modules under the toolchain's own library are not replayed.

    PER CONSTANT, NOT PER MODULE. The first version replayed whole modules and refused genuine
    theorems in `proof_patterns` because a module they import also holds an unrelated `bv_decide`
    lemma; a theorem's verdict should depend only on what it uses. Measured 2026-10-11: ~300
    constants and ~0.3 s per theorem, against ~7 s for `elf_header`'s 16-module closure.

    The replay process is the one whose identity `checkerIdentityProbe` reports, so the checker that
    re-checked is the checker the receipt binds. Running `leanchecker` instead would put a second,
    unidentified executable in that role. -/
def recheckProbe (nonce : String) (theorems : List String) : String :=
  let names := ", ".intercalate (theorems.map fun t => "\"" ++ t ++ "\"")
  ((nativeAxiomClassifierSrc ++ r#"
-- Fresh kernel re-check of each theorem's workspace dependency closure (R-0208).
open Lean in
def _concreteToolchainModule (env : Environment) (sysLib : String)
    (cache : IO.Ref (Std.HashMap Name Bool)) (c : Name) : IO (Option Name) := do
  match env.getModuleIdxFor? c with
  | none => return none
  | some idx =>
    let m := env.header.moduleNames[idx.toNat]!
    let b ← match (← cache.get)[m]? with
      | some b => pure b
      | none => do
        let b := (← findOLean m).toString.startsWith sysLib
        cache.modify (·.insert m b)
        pure b
    return if b then some m else none

open Lean in
def _concreteRecheckClosure (env : Environment) (sysLib : String) (thm : Name)
    : IO (Except String (Std.HashMap Name ConstantInfo × NameSet)) := do
  let cache ← IO.mkRef {}
  let mut todo := #[thm]
  let mut seen : NameSet := {}
  let mut consts : Std.HashMap Name ConstantInfo := {}
  let mut core : NameSet := {}
  while h : todo.size > 0 do
    let c := todo.back
    todo := todo.pop
    if seen.contains c then continue
    seen := seen.insert c
    let some ci := env.find? c | return .error s!"unknown constant {c}"
    if let some m ← _concreteToolchainModule env sysLib cache c then
      core := core.insert m
      continue
    consts := consts.insert c ci
    let mut exprs : Array Expr := #[ci.type]
    if let some e := ci.value? (allowOpaque := true) then exprs := exprs.push e
    match ci with
    | .recInfo v =>
      for r in v.rules do exprs := exprs.push r.rhs
      for n in v.all do todo := todo.push n
    | .inductInfo v =>
      for n in v.all ++ v.ctors do todo := todo.push n
      for n in v.all do
        todo := todo.push (n ++ `rec)
        let mut i := 1
        while env.contains (n ++ Name.mkSimple s!"rec_{i}") do
          todo := todo.push (n ++ Name.mkSimple s!"rec_{i}")
          i := i + 1
    | .ctorInfo v => todo := todo.push v.induct
    | _ => pure ()
    for e in exprs do
      for d in e.getUsedConstants do todo := todo.push d
  return .ok (consts, core)

open Lean in
def _concreteRecheckReplay (core : NameSet) (consts : Std.HashMap Name ConstantInfo)
    : IO (Option String) := do
  try
    let env0 ← importModules (core.toArray.map ({ module := · })) {}
    let _ ← env0.toKernelEnv.replay consts
    return none
  catch e => return some (toString e)

open Lean Elab Command in
#eval show CommandElabM Unit from do
  let env ← getEnv
  let sysLib := ((← Lean.findSysroot) / "lib" / "lean").toString ++ "/"
  for s in ([THEOREMS] : List String) do
    let thm := s.toName
    unless env.contains thm do
      IO.println s!"CONCRETE-RECHECK NONCE {s} unknown"
      continue
    -- AXIOMS ARE ALLOW-LISTED, NOT DENY-LISTED. Lean 4.28's `bv_decide` rested on `Lean.ofReduceBool`;
    -- 4.34's emits a fresh axiom per proof (`…._native.bv_decide.ax_1_41`). A deny-list of the old names
    -- reported such a theorem `rechecked` — the kernel accepts an axiom without proof, so the replay
    -- "succeeded". Only the kernel's standard three may remain; anything else is named.
    let axs ← liftCoreM <| Lean.collectAxioms thm
    let extra := axs.filter (fun a => !(a == ``propext || a == ``Classical.choice || a == ``Quot.sound))
    if extra.any _concreteIsNativeAxiom then
      IO.println s!"CONCRETE-RECHECK NONCE {s} native"
      continue
    if !extra.isEmpty then
      IO.println s!"CONCRETE-RECHECK NONCE {s} failed rests on axioms beyond propext/Classical.choice/Quot.sound: {extra.toList}"
      continue
    match ← _concreteRecheckClosure env sysLib thm with
    | .error e => IO.println s!"CONCRETE-RECHECK NONCE {s} failed {e}"
    | .ok (consts, core) =>
      match ← _concreteRecheckReplay core consts with
      | none => IO.println s!"CONCRETE-RECHECK NONCE {s} rechecked"
      | some why => IO.println s!"CONCRETE-RECHECK NONCE {s} failed {(why.replace "\n" " ").take 300}"
"#).replace "NONCE" nonce).replace "THEOREMS" names

/-- The re-check verdict the run reported for each theorem: EXACTLY one line per theorem, or
    `notRun`. A missing or repeated line is not a verdict. -/
def recheckVerdictsOf (nonce transcript : String) (theorems : List String)
    : List (String × RecheckVerdict) :=
  let lines := taggedLines "CONCRETE-RECHECK" nonce transcript
  theorems.map fun t =>
    let pfx := t ++ " "
    match lines.filter (·.startsWith pfx) with
    | [l] =>
      let rest := String.ofList (l.toList.drop pfx.length)
      (t, if rest == "rechecked" then .rechecked
          else if rest == "native" then .native
          else if rest == "unknown" then .failed "the theorem is not in the replayed environment"
          else if rest.startsWith "failed " then .failed (String.ofList (rest.toList.drop 7))
          else .notRun)
    | _ => (t, .notRun)

/-- Run `lake env lean` on `source` in `ws`, in a scratch directory that is always removed. -/
def runChecker (ws : System.FilePath) (source : String)
    : IO (Except String IO.Process.Output) := do
  let scratchDir ← try
      let d ← IO.Process.output { cmd := "mktemp", args := #["-d"] }
      if d.exitCode != 0 then throw (IO.userError d.stderr)
      pure d.stdout.trimAscii.toString
    catch e => return .error s!"scratch: {e}"
  let scratch := scratchDir ++ "/kernel_replay.lean"
  let out ← try
      IO.FS.writeFile ⟨scratch⟩ source
      pure (Except.ok (← IO.Process.output
        { cmd := "lake", args := #["env", "lean", scratch], cwd := ws
        , env := #[("LAKE_TERM_ANSI", "0")] }))
    catch e => pure (Except.error (toString e))
  let _ ← IO.Process.output { cmd := "rm", args := #["-rf", scratchDir] }
  return out

/-- The identity a finished checker process reported. When it reported none AND exited non-zero,
    the exit status and first stderr line join the reason: "no identity line" alone hides a checker
    that never started (a missing `lean`, a broken toolchain). -/
def checkerResolutionOf (nonce : String) (r : IO.Process.Output) : IO CheckerResolution := do
  match ← checkerIdentityOf nonce (r.stdout ++ "\n" ++ r.stderr) with
  | .unidentified why =>
    if r.exitCode == 0 then return .unidentified why
    let err := ((r.stderr.splitOn "\n").find? (!·.trimAscii.isEmpty)).getD ""
    return .unidentified (s!"{why}; the checker process exited {r.exitCode}"
      ++ (if err.isEmpty then "" else s!": {err.trimAscii}"))
  | ok => return ok

/-- Ask the checker that WOULD run in `ws` who it is, without replaying anything. -/
def probeChecker (ws : System.FilePath) : IO CheckerResolution := do
  let nonce ← freshNonce
  match ← runChecker ws (checkerIdentityProbe nonce) with
  | .error e => return .unidentified s!"the checker could not be run (`lake env lean` in {ws}): {e}"
  | .ok r => checkerResolutionOf nonce r

/-- The checker that WOULD run for this input: its workspace and its toolchain, without running it.

    ONE PRODUCER, used by `replay` and by every consumer that has to compare a stored receipt
    against current material. A receipt binds the toolchain, so checking whether it is still current
    means knowing which toolchain is current — and a status report must be able to ask that without
    paying for a kernel run. Re-reading `lean-toolchain` from somewhere else would be a second answer
    to "which checker", and the two would eventually disagree about whether a receipt had aged.

    The toolchain is read from the RESOLVED WORKSPACE rather than the process directory: it describes
    the checker that would run for this input, and reading it from elsewhere would name a different
    one. -/
private def resolveEnvironment (req : ReplayRequest)
    : IO (Option (System.FilePath × ReplayEnvironment)) := do
  match ← resolveWorkspace req with
  | none => return none
  | some (ws, fromInput) =>
    let toolchain ← try
        let c ← IO.FS.readFile (ws / "lean-toolchain")
        pure c.trimAscii.toString
      catch _ => pure "unknown"
    let digests ← importClosureOf ws req.imports
    return some (ws, { workspace := ws.toString, workspaceFromInput := fromInput
                     , toolchain, imports := req.imports, importDigests := digests })

/-- See above. `toolchain` is the workspace's DECLARED pin, kept for display; `checker` is who would
    actually run, asked of that process. -/
def resolveChecker (req : ReplayRequest) : IO (Option (System.FilePath × ReplayEnvironment)) := do
  match ← resolveEnvironment req with
  | none => return none
  | some (ws, env) => return some (ws, { env with checker := ← probeChecker ws })

/-- The generated Lean file for a request. Pure, so a test can assert what would be checked without
    running a kernel, and so the same bytes are reproducible from the request alone. -/
def ReplayRequest.source (req : ReplayRequest) : String := Id.run do
  let mut s := ""
  for i in req.imports do
    s := s ++ s!"import {i}\n"
  s := s ++ "\n-- Auto-generated kernel replay. Verifies that the referenced Lean theorems exist\n"
  s := s ++ "-- and type-check. Generated from a ReplayRequest; do not edit.\n\n"
  -- ONE `#check` PER DISTINCT THEOREM. Two subjects may legitimately share one proof — the `ghost`
  -- fixture exists to show that a ghost binding does not change the extracted body, so both of its
  -- functions extract identically and link the same theorem.
  --
  -- This was briefly a REFUSAL, on the reasoning that duplicates "cannot be told apart" in a
  -- name-keyed transcript read. That was wrong, and the corpus caught it: they need not be told
  -- apart. A theorem either typechecks or it does not, independently of which subject links it, so
  -- both targets receive the same verdict and that verdict is correct for both. The refusal broke
  -- `--report check-proofs` outright on two fixtures.
  let mut emitted : List String := []
  for t in req.targets do
    if emitted.contains t.theoremName then continue
    emitted := emitted ++ [t.theoremName]
    let subjects := (req.targets.filter (·.theoremName == t.theoremName)).map (·.subject)
    let tag := match t.kind with | .refinement => "" | .ensures => " (ensures)"
    s := s ++ s!"-- {", ".intercalate subjects}{tag}\n#check @{t.theoremName}\n\n"
  return s

/-- Structural checks that must hold before a kernel is invoked at all.

    Each of these would otherwise produce a WRONG verdict rather than an error: an unnamed target
    matches the transcript everywhere, and duplicate theorem names cannot be told apart by a
    name-keyed read of one transcript. -/
def ReplayRequest.validate (req : ReplayRequest) : Except ReplayRefusal Unit := do
  if req.targets.isEmpty then throw .noTargets
  for t in req.targets do
    if t.subject.trimAscii.isEmpty || t.theoremName.trimAscii.isEmpty then
      throw (.unnamedTarget t.subject t.theoremName)
  return ()

/-- Run the kernel over a request and report what it did.

    ONE producer. `concrete --report check-proofs`, receipt minting, the link migration and the test
    gates all ask this function; none of them re-derives the answer, and none of them can obtain a
    verdict without also obtaining the environment that produced it. -/
def replay (req : ReplayRequest) : IO (Except ReplayRefusal ReplayResult) := do
  match req.validate with
  | .error e => return .error e
  | .ok () =>
  -- NOT `resolveChecker`: the checker identity comes from THIS run's own process, below.
  let some (ws, env) ← resolveEnvironment req
    | return .error (.noWorkspace req.inputPath req.fallbackDir)
  let nonce ← freshNonce
  let theorems := (req.targets.map (·.theoremName)).eraseDups
  -- Scratch file. Failure here is a refusal: nothing was checked.
  let scratchDir ← try
      let d ← IO.Process.output { cmd := "mktemp", args := #["-d"] }
      if d.exitCode != 0 then throw (IO.userError d.stderr)
      pure d.stdout.trimAscii.toString
    catch e => return .error (.scratchUnavailable (toString e))
  let scratch := scratchDir ++ "/kernel_replay.lean"
  try
    IO.FS.writeFile ⟨scratch⟩ ("import Lean\n" ++ req.source
      ++ checkerIdentityProbe nonce ++ recheckProbe nonce theorems)
  catch e =>
    let _ ← IO.Process.output { cmd := "rm", args := #["-rf", scratchDir] }
    return .error (.scratchUnavailable (toString e))
  let outcome ← try
      pure (Except.ok (← IO.Process.output
        { cmd := "lake", args := #["env", "lean", scratch], cwd := ws
        , env := #[("LAKE_TERM_ANSI", "0")] }))
    catch e => pure (Except.error (toString e))
  let _ ← IO.Process.output { cmd := "rm", args := #["-rf", scratchDir] }
  let result ← match outcome with
    | .error e => return .error (.checkerUnavailable e)
    | .ok r => pure r
  -- Lean puts errors on either stream.
  let transcript := result.stdout.trimAscii.toString ++ "\n" ++ result.stderr.trimAscii.toString
  let mentions (needle : String) : Bool := (transcript.splitOn needle).length != 1
  -- A per-target verdict is only readable when the checker named the theorems it disliked. When it
  -- exits non-zero having named NONE of them, the file did not compile as a whole and no target has
  -- an individual verdict.
  let named := req.targets.filter (fun t => mentions s!"`{t.theoremName}`")
  let generalFailure := result.exitCode != 0 && named.isEmpty
  let checks := req.targets.map fun t =>
    let verdict :=
      if generalFailure then ReplayVerdict.notAttempted
      else if mentions s!"`{t.theoremName}`" then .rejected
      else match t.binding with
        | .unbound => .acceptedUnbound
        | .bound   => .accepted
    { target := t, verdict }
  let checker ← checkerResolutionOf nonce result
  let recheck := recheckVerdictsOf nonce transcript theorems
  return .ok { environment := { env with checker }, checks, exitCode := result.exitCode
             , generalFailure, transcript, recheck }

end Concrete.Proof
