#!/usr/bin/env bash
# KERNEL REPLAY AS A TYPED SERVICE (R-0004 package 3).
#
# "Did the Lean kernel accept this?" is the question every replay-backed receipt rests on. Until
# 2026-08-16 it had no answerer, only a rendering: the check lived INLINE in `compileAndReport`,
# entangled with that function's `reportJson` parameter, and every failure path PRINTED and returned
# an exit code. Nothing else could ask — not receipt minting, not the link migration, not a test —
# so a second caller would have had to write a second answer to the same question.
#
# This gate holds the extracted producer to the properties that make its answer usable as evidence
# rather than as output. Two of them are the whole point and are asserted in both directions:
#
#   * A REFUSAL IS A VALUE. "No kernel ran" and "the kernel said no" are opposite facts, and a
#     minting path that cannot tell them apart records a refusal as a pass.
#   * AN EMPTY REQUEST IS REFUSED, NOT SATISFIED. `checks.all accepted` is TRUE over an empty list.
#     A vacuous "all accepted" is precisely the answer a minting authority must never receive.
#
# Every negative below is paired with a live positive control, because a producer that refused
# everything would satisfy the negatives alone.
set -uo pipefail
source "$(dirname "${BASH_SOURCE[0]}")/lib/gate_env.sh" || exit 2
trap 'rc=$?; if [ "$rc" -ne 0 ] && [ "${GATE_DONE:-0}" -ne 1 ]; then
  echo "FATAL: unexpected shell failure (exit $rc) — the verdict below is not trustworthy" >&2; exit "$rc"; fi' ERR
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
# PRIVATE CHECKOUT (R-0546, lib/private_checkout.sh): this gate creates an ensures/exit control directory inside the checkout,
# which a full pass shares with every other gate; it runs in a private copy instead.
source "$ROOT_DIR/scripts/tests/lib/private_checkout.sh"
reexec_in_private_checkout "${BASH_SOURCE[0]}" "$@"
source "scripts/tests/lib/fresh.sh"
require_fresh_binary || exit 1
TMP="$(mktemp -d)"; trap 'rm -rf "$TMP"' EXIT
BIN="$ROOT_DIR/.lake/build/bin/concrete"

PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }

# probe <expected-substring> <label> <lean-body>
probe() {
  cat > "$TMP/p.lean" <<LEAN
import Concrete
open Concrete.Proof
def tgt : ReplayTarget :=
  { subject := "elf.parse", theoremName := "Concrete.Proof.parse_byte_correct"
  , kind := .refinement, origin := .sourceLinked, binding := .bound }
$3
LEAN
  local out; out="$(lake env lean "$TMP/p.lean" 2>&1 || true)"
  if grep -qE "error:|error\(lean" <<<"$out"; then
    no "$2 — probe did not elaborate: $(tr '\n' ' ' <<<"$out" | cut -c1-200)"
  elif grep -qF -- "$1" <<<"$out"; then ok "$2"
  else no "$2 — got: $(tr '\n' ' ' <<<"$out" | cut -c1-200)"; fi
}

echo "=== the producer answers, and the answer is not vacuous ==="

# THE POSITIVE CONTROL FOR EVERYTHING BELOW. Without it, a producer that refused every request would
# satisfy every refusal assertion in this file.
probe "CONTROL accepted general=false" "a real request is ACCEPTED (the control the refusals are measured against)" \
'#eval show IO Unit from do
  match ← replay { inputPath := "Main.lean", imports := ["Concrete"], targets := [tgt] } with
  | .error e => IO.println s!"CONTROL-REFUSED {e.canonical}"
  | .ok r =>
    let v := ((r.verdictFor tgt.theoremName).map (·.canonical)).getD "?"
    IO.println s!"CONTROL {v} general={r.generalFailure}"'

# THE ENVIRONMENT TRAVELS WITH THE VERDICT. A verdict that cannot say which checker produced it
# cannot be invalidated later: when the toolchain moves, nothing knows the old answer is stale.
probe "ENV ok" "the result retains workspace, toolchain and import closure" \
'#eval show IO Unit from do
  match ← replay { inputPath := "Main.lean", imports := ["Concrete"], targets := [tgt] } with
  | .error _ => IO.println "ENV refused"
  | .ok r =>
    let e := r.environment
    IO.println (if !e.workspace.isEmpty && !e.toolchain.isEmpty && e.toolchain != "unknown"
                   && e.imports == ["Concrete"] && e.workspaceFromInput
                then "ENV ok" else s!"ENV incomplete: {e.workspace}|{e.toolchain}|{e.imports}")'

echo "=== a whole-file failure is NOT reported as every theorem failing ==="

# THE DEFECT THIS EXTRACTION FIXED. The inline version computed
#     generalFailure := exitCode != 0 && failed.isEmpty
# AFTER a fallback that filled `failed` with every target — so the flag was unreachably false, the
# "Lean check failed (exit code N)" disclosure and the `env_failure` JSON status were dead code, and
# a file that did not compile was reported as every theorem individually "not found". That is the
# same misdiagnosis the workspace-resolution fix exists to prevent: blaming artifacts that were fine.
#
# An import that does not exist makes the FILE fail while naming no theorem. Only reachable as a test
# because `imports` is a request FIELD rather than a constant.
probe "BADIMPORT general=true verdict=not_attempted rejected=0" \
  "a file that does not compile yields generalFailure and NOT-ATTEMPTED, blaming no theorem" \
'#eval show IO Unit from do
  match ← replay { inputPath := "Main.lean", imports := ["NoSuchModule"], targets := [tgt] } with
  | .error e => IO.println s!"BADIMPORT-REFUSED {e.canonical}"
  | .ok r =>
    let v := ((r.verdictFor tgt.theoremName).map (·.canonical)).getD "?"
    IO.println s!"BADIMPORT general={r.generalFailure} verdict={v} rejected={r.rejected.length}"'

# ...and such a run is NOT acceptable to a minting authority. `allAccepted` must fail closed on a
# general failure even though no individual target was rejected.
probe "BADIMPORT allAccepted=false fullyBound=false" \
  "a general failure is not acceptable and not fully bound (fail closed)" \
'#eval show IO Unit from do
  match ← replay { inputPath := "Main.lean", imports := ["NoSuchModule"], targets := [tgt] } with
  | .error _ => IO.println "refused"
  | .ok r => IO.println s!"BADIMPORT allAccepted={r.allAccepted} fullyBound={r.fullyBound}"'

# THE DISCRIMINATION THAT MAKES THE ABOVE MEAN SOMETHING: a theorem that genuinely does not exist IS
# individually rejected, and the run is NOT a general failure. Without this, "everything becomes
# notAttempted" would also pass.
probe "MISSING general=false verdict=rejected" \
  "a theorem that does not exist is REJECTED individually, not swept into a general failure" \
'#eval show IO Unit from do
  let bad : ReplayTarget := { tgt with theoremName := "Concrete.Proof.no_such_theorem_at_all" }
  match ← replay { inputPath := "Main.lean", imports := ["Concrete"], targets := [bad] } with
  | .error e => IO.println s!"MISSING-REFUSED {e.canonical}"
  | .ok r =>
    let v := ((r.verdictFor bad.theoremName).map (·.canonical)).getD "?"
    IO.println s!"MISSING general={r.generalFailure} verdict={v}"'

echo "=== accepted-but-unbound is not accepted ==="

# A theorem can type-check perfectly while the claim it is attached to has no stored subject digest,
# in which case the freshness comparison has nothing to compare against and a changed body goes
# undetected. On examples/hmac_sha256 this once read "11 verified" while ProofCore was concurrently
# emitting 11 "this claim is unbound, not proved" errors. The distinction is in the TYPE so the two
# cannot be summed.
probe "UNBOUND verdict=accepted_unbound allAccepted=true fullyBound=false" \
  "an unbound claim whose theorem type-checks is accepted_unbound — never accepted" \
'#eval show IO Unit from do
  let u : ReplayTarget := { tgt with binding := .unbound }
  match ← replay { inputPath := "Main.lean", imports := ["Concrete"], targets := [u] } with
  | .error e => IO.println s!"UNBOUND-REFUSED {e.canonical}"
  | .ok r =>
    let v := ((r.verdictFor u.theoremName).map (·.canonical)).getD "?"
    IO.println s!"UNBOUND verdict={v} allAccepted={r.allAccepted} fullyBound={r.fullyBound}"'

echo "=== every failure class is a VALUE, not a printed line ==="

# AN EMPTY REQUEST IS REFUSED. This is the load-bearing one: `List.all` is TRUE over an empty list,
# so a producer that returned an empty success would hand a minting authority a vacuous "everything
# was accepted". Refusing forces every caller to decide what nothing-to-check means.
probe "EMPTY-REFUSED no_targets" "an empty request REFUSES rather than reporting vacuous success" \
'#eval show IO Unit from do
  match ← replay { inputPath := "Main.lean", imports := ["Concrete"], targets := [] } with
  | .error e => IO.println s!"EMPTY-REFUSED {e.canonical}"
  | .ok r => IO.println s!"EMPTY-ACCEPTED allAccepted={r.allAccepted} (vacuous)"'

# TWO SUBJECTS MAY SHARE ONE PROOF, and both must receive its verdict.
#
# This was briefly a REFUSAL, on the reasoning that duplicates "cannot be told apart" in a name-keyed
# transcript read. That was wrong and the corpus caught it: they need not be told apart, because a
# theorem either typechecks or it does not, independently of which subject links it. The refusal
# broke `--report check-proofs` outright on `proof_patterns/ghost` and
# `proof_patterns/stale_missing_partial`, two fixtures where distinct functions extract identically
# and legitimately link the same theorem. One `#check` is emitted per DISTINCT theorem.
probe "SHARED both=accepted checks=2 emitted=1" \
  "two subjects sharing one theorem both get its verdict, from a single emitted check" \
'#eval show IO Unit from do
  let a := tgt
  let b := { tgt with subject := "elf.other" }
  let req : ReplayRequest := { inputPath := "Main.lean", imports := ["Concrete"], targets := [a, b] }
  let emitted := ((req.source.splitOn "#check").length) - 1
  match ← replay req with
  | .error e => IO.println s!"SHARED-REFUSED {e.canonical}"
  | .ok r =>
    let vs := r.checks.map (·.verdict.canonical)
    let both := if vs == ["accepted", "accepted"] then "accepted" else String.intercalate "/" vs
    IO.println s!"SHARED both={both} checks={r.checks.length} emitted={emitted}"'

# An empty needle matches the transcript everywhere, so an unnamed target would report as accepted
# WITHOUT BEING CHECKED — a false pass, which is the worst available failure mode.
probe "UNNAMED-REFUSED unnamed_target" "a target with an empty name is refused (an empty needle matches everywhere)" \
'#eval show IO Unit from do
  let anon : ReplayTarget := { tgt with theoremName := "  " }
  match ← replay { inputPath := "Main.lean", imports := ["Concrete"], targets := [anon] } with
  | .error e => IO.println s!"UNNAMED-REFUSED {e.canonical}"
  | .ok r =>
    let v := ((r.verdictFor anon.theoremName).map (·.canonical)).getD "?"
    IO.println s!"UNNAMED-ACCEPTED {v}"'

# NO WORKSPACE: both candidates outside any Lake tree. The theorems were never looked at, so the
# refusal must name the workspace rather than the theorems.
probe "NOWS-REFUSED no_workspace" "no workspace anywhere refuses by NAME, blaming no theorem" \
'#eval show IO Unit from do
  match ← replay { inputPath := "/tmp", fallbackDir := "/tmp", imports := ["Concrete"], targets := [tgt] } with
  | .error e => IO.println s!"NOWS-REFUSED {e.canonical}"
  | .ok _ => IO.println "NOWS-ACCEPTED"'

# ...and the refusal SAYS SO in the words the freshness gate reads.
probe "cannot locate a Lake workspace" "the no-workspace refusal explains itself" \
'#eval IO.println (ReplayRefusal.noWorkspace "/tmp/x" "/tmp").explain'

echo "=== the generated source is a pure function of the request ==="

# Reproducibility: the same request must produce the same bytes, and a DIFFERENT request must not.
# Without the second half, a constant generator would pass the first.
probe "SRC stable=true distinct=true" "the generated file is reproducible from the request, and varies with it" \
'#eval do
  let a : ReplayRequest := { inputPath := "x", imports := ["Concrete"], targets := [tgt] }
  let b : ReplayRequest := { a with targets := [{ tgt with theoremName := "Other.thm" }] }
  IO.println s!"SRC stable={a.source == a.source} distinct={a.source != b.source}"'

echo "=== the CLI renders the producer's answer and derives none of it ==="

# The extraction must not have moved any verdict. These are the numbers the report gave before it.
CP="$("$BIN" examples/elf_header/src/main.con --report check-proofs 2>&1 || true)"
if grep -q '5 verified, 0 failed' <<<"$CP"; then
  ok "elf_header still reports 5 verified, 0 failed through the producer"
else
  no "elf_header moved: $(grep -o 'Summary:.*' <<<"$CP" | head -1)"
fi
if grep -q 'Workspace: .*(from input)' <<<"$CP" && grep -q 'Toolchain: leanprover' <<<"$CP"; then
  ok "the report still names the workspace and toolchain that produced the verdict"
else
  no "the report no longer names its environment — the verdict is anonymous again"
fi
# The unbound population still reports separately from the verified one.
HM="$("$BIN" examples/hmac_sha256/src/main.con --report check-proofs 2>&1 || true)"
if grep -q '0 verified, 0 failed; 11 unbound (type-checked, NOT proved)' <<<"$HM"; then
  ok "hmac_sha256 still separates 11 unbound from verified"
else
  no "hmac_sha256 unbound accounting moved: $(grep -o 'Summary:.*' <<<"$HM" | head -1)"
fi
# ONE PRODUCER: the CLI must not carry its own workspace walker any more.
if grep -q 'partial def findLakeWorkspace' Main.lean; then
  no "Main.lean still defines its own findLakeWorkspace — two answers to 'where does this replay'"
else
  ok "the CLI has no second workspace resolver"
fi

echo "=== a successful replay is the ONLY minting authority ==="

# THE PROPERTY, checked as a COMPILE failure rather than a runtime refusal. `ProofEvidenceReceipt`
# has a private constructor and one producer, `mint`, whose first argument is a `SuccessfulReplay`.
# That token has a private constructor and one producer, `of?`, which takes a `ReplayResult` — which
# also has a private constructor and whose only producer is `replay`. There is therefore no term a
# caller can write that claims a kernel ran. Asserting this at runtime would be the wrong level: a
# runtime refusal is a check that can be forgotten, and the point of the chain is that it cannot be.
cat > "$TMP/nomint.lean" <<'LEAN'
import Concrete
open Concrete.Proof
def ev : EdgeEvidence := { edge := .body, tables := [], tableDigests := [], quantifiesOverTable := true }
-- Assembling the token directly: the constructor is private.
def forged : SuccessfulReplay :=
  { theoremName := "Fake.thm", subject := "s"
  , environment := { workspace := "/ws", workspaceFromInput := true
                   , toolchain := "lean", imports := [] } }
LEAN
if lake env lean "$TMP/nomint.lean" >/dev/null 2>&1; then
  no "a SuccessfulReplay can be assembled directly — unchecked facts still reach a receipt"
else
  ok "a SuccessfulReplay cannot be assembled; only a real replay produces one"
fi

# ...and the same for the result it is extracted from, or the chain would just move one link along.
cat > "$TMP/noresult.lean" <<'LEAN'
import Concrete
open Concrete.Proof
def forgedResult : ReplayResult :=
  { environment := { workspace := "/ws", workspaceFromInput := true
                   , toolchain := "lean", imports := [] }
  , checks := [], exitCode := 0, generalFailure := false, transcript := "" }
LEAN
if lake env lean "$TMP/noresult.lean" >/dev/null 2>&1; then
  no "a ReplayResult can be assembled directly — a forged run still mints"
else
  ok "a ReplayResult cannot be assembled; the replay producer is its only source"
fi

# THE POSITIVE CONTROL. Without it the two refusals above would also hold if minting were simply
# broken. A real replay produces a token, and the receipt records the theorem the KERNEL accepted
# together with the toolchain it ran under — neither of which is a caller's assertion any more.
probe "MINTED artifact=Concrete.Proof.parse_byte_correct toolchain=bound" \
  "a real replay mints, and the receipt takes its artifact and toolchain from the run" \
'#eval show IO Unit from do
  let ev : EdgeEvidence := { edge := .body, tables := [], tableDigests := [], quantifiesOverTable := true }
  match ← replay { inputPath := "Main.lean", imports := ["Concrete"], targets := [tgt] } with
  | .error e => IO.println s!"REPLAY-REFUSED {e.canonical}"
  | .ok r =>
    match SuccessfulReplay.of? r tgt.theoremName with
    | .error e => IO.println s!"TOKEN-REFUSED {e.canonical}"
    | .ok sr =>
      match ReceiptMaterial.of? (some "v2:s") ev "ROOT" false [] "cv" "ws" "cx" "im" with
      | none => IO.println "MATERIAL-REFUSED"
      | some m =>
        let rc := ProofEvidenceReceipt.mint sr m
        -- The CHECKER THAT RAN (R-0208), not the lean-toolchain text: the token carries the identity
        -- the replay process reported about itself, and the receipt must bind exactly that.
        let bound := rc.toolchainId == toolchainIdOf "cv" sr.checker.canonical
          && rc.toolchainId != toolchainIdOf "cv" sr.environment.toolchain
        IO.println s!"MINTED artifact={rc.theoremArtifact} toolchain={if bound then "bound" else "invented"}"'

# AN UNBOUND ACCEPTANCE IS NOT A MINTING AUTHORITY. The kernel accepted the theorem; the claim has
# no stored proof-subject digest, so a receipt minted from it would record freshness against nothing.
probe "TOKEN-REFUSED not_accepted" "an accepted-but-UNBOUND claim yields no token" \
'#eval show IO Unit from do
  let u : ReplayTarget := { tgt with binding := .unbound }
  match ← replay { inputPath := "Main.lean", imports := ["Concrete"], targets := [u] } with
  | .error e => IO.println s!"REPLAY-REFUSED {e.canonical}"
  | .ok r =>
    match SuccessfulReplay.of? r u.theoremName with
    | .error e => IO.println s!"TOKEN-REFUSED {e.canonical}"
    | .ok _ => IO.println "TOKEN-GRANTED (laundering)"'

# AN INTERRUPTED RUN IS NOT A SUCCESSFUL ONE. Under a general failure no verdict means anything, and
# the token must refuse before it ever looks at an individual check.
probe "TOKEN-REFUSED under_general_failure" "a run whose file did not compile yields no token" \
'#eval show IO Unit from do
  match ← replay { inputPath := "Main.lean", imports := ["NoSuchModule"], targets := [tgt] } with
  | .error e => IO.println s!"REPLAY-REFUSED {e.canonical}"
  | .ok r =>
    match SuccessfulReplay.of? r tgt.theoremName with
    | .error e => IO.println s!"TOKEN-REFUSED {e.canonical}"
    | .ok _ => IO.println "TOKEN-GRANTED (interrupted run treated as success)"'

# SILENCE IS NOT A VERDICT. A theorem absent from the request has no verdict in this run, and
# treating "no news" as acceptance is how an unreplayed artifact acquires a receipt.
probe "TOKEN-REFUSED not_replayed" "a theorem that was not part of the run yields no token" \
'#eval show IO Unit from do
  match ← replay { inputPath := "Main.lean", imports := ["Concrete"], targets := [tgt] } with
  | .error e => IO.println s!"REPLAY-REFUSED {e.canonical}"
  | .ok r =>
    match SuccessfulReplay.of? r "Concrete.Proof.some_other_theorem" with
    | .error e => IO.println s!"TOKEN-REFUSED {e.canonical}"
    | .ok _ => IO.println "TOKEN-GRANTED (silence read as acceptance)"'

# A FALLBACK WORKSPACE MAY REPLAY BUT MAY NOT MINT. A receipt is durable evidence that must be
# re-checkable from the artifact alone; a verdict that depended on where the caller stood is not a
# property of the program. Replaying such an input stays legitimate — check_purecore_proofs.sh relies
# on it — and only the minting step refuses.
probe "TOKEN-REFUSED fallback_workspace" "a workspace resolved from the caller's directory cannot mint" \
'#eval show IO Unit from do
  match ← replay { inputPath := "/tmp", fallbackDir := ".", imports := ["Concrete"], targets := [tgt] } with
  | .error e => IO.println s!"REPLAY-REFUSED {e.canonical}"
  | .ok r =>
    match SuccessfulReplay.of? r tgt.theoremName with
    | .error e => IO.println s!"TOKEN-REFUSED {e.canonical}"
    | .ok _ => IO.println "TOKEN-GRANTED (location-dependent evidence minted)"'

echo "=== a refused contract-discharge theorem fails the build ==="

# A rejected `ensures_proof` was rendered with an X and then exited 0, so `--report check-proofs`
# reported SUCCESS on a program whose `#[ensures]` obligation had no accepted proof. The counts are
# refinement-only by design — gates extract "N verified, M failed" by exact string — so the hole was
# invisible in every number the report prints.
#
# The broken fixture is built in a scratch copy INSIDE the workspace rather than added to the corpus:
# a permanently-broken example would move the pinned coverage denominators that several other gates
# assert exactly, to test a property that needs only one run.
CTRL_DIR="$ROOT_DIR/.ensures-exit-probe"
rm -rf "$CTRL_DIR"; mkdir -p "$CTRL_DIR"
cp -r examples/constant_time_tag "$CTRL_DIR/ct"
CT="$CTRL_DIR/ct/src/main.con"

# POSITIVE CONTROL FIRST: unmodified, the fixture must exit 0. Without it, "the broken one exits 1"
# is satisfied by check-proofs failing on everything.
# Guarded with `if`, not a bare call: the ERR trap fires on any non-zero simple command, and a
# non-zero exit is exactly what the negative below is testing for.
if "$BIN" "$CT" --report check-proofs >/dev/null 2>&1; then
  ok "the unmodified fixture still exits 0 (the negative below is measured against this)"
else
  no "the unmodified fixture already exits non-zero — the ensures control would be vacuous"
fi

sed -i 's/#\[ensures_proof(Examples\.ConstantTimeTag\.Proofs\.ct_compare_different_tag_correct)\]/#[ensures_proof(Examples.ConstantTimeTag.Proofs.no_such_discharge_theorem)]/' "$CT"
if OUT="$("$BIN" "$CT" --report check-proofs 2>&1)"; then RC=0; else RC=$?; fi
rm -rf "$CTRL_DIR"
if [ "$RC" -ne 0 ]; then
  ok "a refused ensures-discharge theorem makes check-proofs exit non-zero"
else
  no "an ensures theorem the kernel refused still exits 0 — the report marks it and the build passes"
fi
if grep -q 'no_such_discharge_theorem: theorem not found (counted as failed)' <<<"$OUT" \
   && grep -qE 'Summary: [0-9]+ verified, [1-9][0-9]* failed' <<<"$OUT"; then
  ok "...and the refusal is rendered in the contract-obligations section AND counted as failed (bug 089)"
else
  no "the ensures section vanished, so the exit code is the only signal left"
fi

GATE_DONE=1
echo "REPLAY-PRODUCER: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
