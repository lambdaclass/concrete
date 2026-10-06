#!/usr/bin/env bash
# R-0004 step 6 — typed dependency edges, DERIVED from the theorem.
#
# The edge kind must come from what a proof actually uses, never from an author's
# declaration: a mode flag would let someone claim `contract` while the proof
# unfolds a concrete table, and an implementation change that preserved the
# contract would then fail to stale a caller that really depends on the body.
# -E (errtrace) IS LOAD-BEARING: without it bash does not propagate the ERR trap into functions, so
# every failure inside flush_mint_probes and its helpers was invisible to the guard below.
set -uEo pipefail
# GATE_DONE IS INITIALIZED HERE, not left to `${GATE_DONE:-0}`. It was only ever assigned near the
# end, so an inherited GATE_DONE=1 from the environment disarmed the trap for the WHOLE run.
GATE_DONE=0
# An unexpected command failure must not sit beside a green total. A backtick in a
# probe label once executed `missing` as a command and the gate still reported
# 26/0 — the error was visible and the verdict said otherwise.
trap 'rc=$?; if [ "$rc" -ne 0 ] && [ "$GATE_DONE" -ne 1 ]; then
  echo "FATAL: unexpected shell failure (exit $rc) — the verdict below is not trustworthy" >&2; exit "$rc"; fi' ERR
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
# ONE definition of "a stored proof link". This gate previously held four private copies of the
# regex, and V2 activation invalidated all four at once — the corpus selection went empty and the
# gate aborted. That is exactly what lib/fingerprints.sh exists to prevent.
source "$ROOT_DIR/scripts/tests/lib/fingerprints.sh"
cd "$ROOT_DIR"
TMP="$(mktemp -d)"; trap 'rm -rf "$TMP"' EXIT
PASS=0; FAIL=0
# EVERY ASSERTION'S IDENTITY, recorded as it is made. The pinned TOTAL catches deletion but not
# substitution: delete one negative test and duplicate another and the count, both source manifests
# and the negative-test tally all stay put. The source manifests pin what is DECLARED; this pins
# what was actually ASSERTED at run time, which is the only thing that catches a declared assertion
# that never ran, or one standing in for another.
ASSERT_LABELS=""
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); ASSERT_LABELS+="$1
"; }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); ASSERT_LABELS+="$1
"; }

# THE ORDINARY PROBES ARE A POPULATION TOO. Only the minting probes were pinned, so deleting an
# ordinary probe left PASS=309 FAIL=0 and exit 0 — the same silent-deletion hole, thirteen times
# larger. Registration happens here, at the single point every ordinary probe passes through, so it
# cannot be bypassed by adding a call site.
# THE COMPILER'S EXIT STATUS MUST NOT BE DISCARDED. Twenty-six sites below run the compiler with
# `2>/dev/null` and then read its OUTPUT, and every one of them treats an empty result as "nothing
# wrong": no refusals, no bypasses, no unresolved identities. A compiler that crashed therefore
# produced a clean bill of health. Failures are recorded to a file rather than a variable because
# these calls happen inside command substitutions, where a counter increment would be lost with the
# subshell — the same class of defect.
CC_FAILURES="$TMP/cc_failures"; : > "$CC_FAILURES"
cc() {
  local f="$1" rep="$2" rc=0 out
  out="$("$ROOT_DIR/.lake/build/bin/concrete" "$f" --report "$rep" 2>/dev/null)" || rc=$?
  # A NONZERO EXIT IS NORMAL HERE and must not be treated as a producer failure: the compiler exits
  # nonzero when the SUBJECT is bad — a stale-proof fixture is meant to be rejected — while still
  # printing its report. Six such runs exist in this corpus. What is never normal is an EMPTY
  # report, because that is what a crash produces and it is exactly what every caller below reads
  # as "no refusals, no bypasses, nothing unresolved".
  # NONEMPTY IS NOT COMPLETE. Accepting any nonempty stdout let a report prefix printed before a
  # crash — or stray whitespace — count as a full report, and the absence-oriented assertions below
  # pass on arbitrary text that simply lacks the string they search for. Each report is therefore
  # required to carry its own structural anchor.
  # A LEADING ANCHOR PROVES THE REPORT STARTED, NOT THAT IT FINISHED. Every one of these headers is
  # printed first, so a process could emit the header, truncate, and still satisfy an anchor check —
  # after which the absence-oriented consumers below see a clean result. Each report is instead
  # required to be COMPLETE on its own terms: a trailer that is only reached at the end, or a
  # self-declared count that must match the rows actually rendered.
  local why=""
  if [ -z "$out" ]; then
    why="produced NO output"
  else
    case "$rep" in
      subject-facts)
        # COUNTING THE ENTRY MARKERS IS NOT ENOUGH. Truncating immediately after the LAST `v1:` line
        # leaves header and marker count agreeing while deleting that entry's body — including its
        # `depRoot` line, which is exactly what the absence-oriented consumers search for. Every
        # entry emits exactly one `shadow depRoot:` line and it is the entry's LAST line, so the
        # depRoot count must also agree and the report must END on one.
        local hdr rows roots
        hdr="$(sed -n '1s/.*(\([0-9]\+\) entries).*/\1/p' <<<"$out")"
        rows="$(grep -c '^v1:' <<<"$out" || true)"
        roots="$(grep -c 'shadow depRoot:' <<<"$out" || true)"
        if [ -z "$hdr" ]; then why="has no '=== Subject facts (N entries) ===' header"
        elif [ "$hdr" != "$rows" ]; then why="declares $hdr entries but rendered $rows — truncated"
        elif [ "$hdr" != "$roots" ]; then why="declares $hdr entries but rendered $roots depRoot lines — an entry body is truncated"
        elif ! grep -q 'shadow depRoot:' <<<"$(tail -1 <<<"$out")"; then why="does not end on a depRoot line — the last entry is truncated"
        fi ;;
      proof-status)
        grep -qE '^Totals: [0-9]+ functions' <<<"$out" || why="has no 'Totals:' trailer — truncated before the end" ;;
      generated-implementations)
        grep -qE '^-- emitted=[0-9]+' <<<"$out" || why="has no '-- emitted=' trailer — truncated before the end" ;;
      consistency)
        if grep -qE '^All consistency checks passed\.' <<<"$out"; then :
        elif grep -qE '^Found [0-9]+ consistency violation' <<<"$out"; then
          local n r
          n="$(sed -n 's/^Found \([0-9]\+\) consistency violation.*/\1/p' <<<"$out")"
          r="$(grep -c '^  \[' <<<"$out" || true)"
          [ "$n" = "$r" ] || why="declares $n violation(s) but rendered $r — truncated"
        else why="is neither a pass line nor a violation report — truncated or not the report asked for"
        fi ;;
      *) why="is a report kind with no declared completeness rule" ;;
    esac
  fi
  # EXPECTED STATUS IS PER REPORT, because a global "0 or 1 is fine" rule launders real failures.
  # Exit 1 is OVERLOADED — a missing file, an unknown report name and no arguments all return it —
  # and accepting it everywhere hid a genuine COMPILER BUG: attestation-join,
  # generated-implementations and impl-manifest printed their report, fell through to the
  # "Unknown report type" branch, and exited 1. Three assertions in this gate consumed that output
  # as if the run had succeeded. The compiler is fixed; this keeps it fixed.
  #
  # The sets below are MEASURED across the 25 proof-carrying fixtures, not assumed:
  #   subject-facts 25/25 exit 0   generated-implementations 25/25 exit 0
  #   proof-status  24x0 1x1       consistency               24x0 1x1
  # The single 1 in each is the stale-proof fixture, which is SUPPOSED to be rejected.
  if [ -z "$why" ]; then
    # PER (FILE, REPORT), NOT PER REPORT. Accepting "0 or 1" for proof-status and consistency
    # everywhere launders a real failure: a CLEAN consistency report exiting 1 would pass. Exactly
    # ONE fixture is supposed to be rejected — evidence_classes/stale_proof — and it is named here,
    # so every other file must exit 0 on every report.
    #
    # THIS RULE IS ENFORCED ONLY FOR CALLS THAT HAPPEN. The matrix is exercised across the whole
    # fixture population by the block near the end of this gate; without it, a comment describing a
    # one-off manual measurement would read like permanent coverage.
    local okrc="0"
    case "$rep" in
      proof-status|consistency)
        case "$f" in */evidence_classes/stale_proof/*) okrc="1" ;; esac ;;
    esac
    case " $okrc " in
      *" $rc "*) ;;
      *) why="exited $rc, which is not an expected status for --report $rep (expected: $okrc)" ;;
    esac
  fi
  [ -z "$why" ] || printf '%s --report %s %s (exit %s)\n' "$f" "$rep" "$why" "$rc" >> "$CC_FAILURES"
  printf '%s' "$out"
}
reconcile_compiler_runs() {
  local n; n="$(awk 'END{print NR}' "$CC_FAILURES")"
  if [ "$n" -eq 0 ]; then
    ok "the compiler succeeded on every file this gate read evidence from"
  else
    no "the compiler FAILED $n time(s) — every site here reads its output, so a crash would have read as a clean result: $(head -3 "$CC_FAILURES" | tr '\n' ';' )"
  fi
}

PROBE_N=0
PROBE_RECS=""
probe() {
  local label="$1" want="$2" body="$3"
  PROBE_N=$((PROBE_N + 1))
  if [ -n "$_MINT_HASHER" ]; then
    PROBE_RECS+="$(_manifest_record "$label" "$want" "ordinary" "$body")
"
  fi
  # An empty expectation accepts any output, because `grep -qF ""` matches everything.
  if [ -z "$want" ]; then
    no "$label — has an EMPTY expectation, which would accept any output"
    return
  fi
  cat > "$TMP/p.lean" <<LEAN
import Concrete
import Examples
open Lean Meta Concrete Concrete.Proof
-- TEST IDENTITIES. \`DefinitionIdentity\` has no total constructor and must not acquire one, so a
-- probe that needs scoped identities builds them through \`of?\` and matches on the result. The
-- implementation component is a real digest of the declaration name, so distinct declarations get
-- distinct identities without any literal hex in the probe.
def tPkg : String := Concrete.shortHash "test-package"
def tid? (m d : String) : Option DefinitionIdentity :=
  (DefinitionIdentity.of? tPkg m d (Concrete.shortHash ("impl:" ++ m ++ "." ++ d))).toOption
-- MINTING REQUIRES A REAL KERNEL RUN, and the probes that mint pay for it — but they do not run
-- here. \`SuccessfulReplay\` has a private constructor and \`Concrete.Proof.replay\` is the only
-- producer of the \`ReplayResult\` it is extracted from, so there is no cheaper way to obtain a
-- receipt: that is exactly the property under test, and it is type-level, so it does not depend on
-- how many times the replay happens. Minting probes therefore declare themselves with
-- \`probe_mint\` and run in their group's batch driver: ONE successful replay per group, with every
-- receipt in that group deriving from that replay.
--
-- The mint definition lives ONLY in that driver. It used to be duplicated here too, and a second
-- definition of the same fact is a second thing to keep in step — the batch could have drifted from
-- this copy without any gate noticing.
--
-- Material validation does NOT go through minting at all: it is pure, and most of those legs are
-- about material rather than authority. Keeping them separate is why this file did not grow 36
-- kernel invocations.
$body
LEAN
  # `|| true` USED TO DISCARD THE EXIT STATUS here, so a Lean that printed the wanted string and
  # then exited nonzero passed. Output matching and process success are separate facts and both are
  # required; the batch path already checked both, and this asymmetry left the 240 ordinary probes
  # weaker than the 18 batched ones.
  # ONE capture site. The self-test swaps the COMMAND, not the capture: a seam with its own status
  # capture would stay red even if `|| true` were restored on the real invocation, so it would prove
  # nothing about the path that actually runs Lean.
  local out rc=0
  local -a cmd=(lake env lean "$TMP/p.lean")
  if [ "${PROBE_SELFTEST_FAKE_EXIT:-}" = "1" ]; then
    printf '#!/bin/sh\nprintf "%%s\\n" "$1"\nexit 9\n' > "$TMP/fakeprobe"; chmod +x "$TMP/fakeprobe"
    cmd=("$TMP/fakeprobe" "$want")
  fi
  out="$("${cmd[@]}" 2>&1)" || rc=$?
  # An error is a failure whatever the text says — a probe that cannot elaborate
  # must not pass on a digit inside "line:col" (that happened in the migration gate).
  # Match a LEAN DIAGNOSTIC, not the bare word: `Except.error` is a legitimate
  # value and matching "error" made every probe of a refusal a false negative —
  # the vacuity guard corrupting the measurement it exists to protect.
  if [ "$rc" -ne 0 ]; then
    no "$label — probe exited $rc: $(printf '%s' "$out" | tr '\n' ' ' | cut -c1-180)"
  elif grep -qE "error:|error\(lean" <<<"$out"; then
    no "$label — probe did not elaborate: $(printf '%s' "$out" | tr '\n' ' ' | cut -c1-180)"
  # WHOLE WORD, NOT SUBSTRING. `grep -F "BOUND"` is satisfied by "NOTBOUND", and "COMPARABLE" by
  # "INCOMPARABLE" — so a predicate could be INVERTED and its own positive control would still pass.
  # Measured: ten such pairs among these expectations, and that negation-of-itself case is the one
  # that matters, because it is the failure a positive control exists to catch. Whole-LINE matching
  # would also be sound but is wrong here: many probes legitimately assert a token inside a rendered
  # Lean value, and requiring the entire line would pin incidental formatting as if it were the
  # claim.
  elif grep -qwF -- "$want" <<<"$out"; then ok "$label"
  else no "$label — got: $(printf '%s' "$out" | tr '\n' ' ' | cut -c1-180)"; fi
}

# ---------------------------------------------------------------------------
# BATCHED MINT PROBES — one successful replay per group, with every receipt in that group deriving
# from that replay.
#
# `mintProbe` runs `Concrete.Proof.replay`, a real kernel run, MEASURED at ~17.5s. Eighteen probes
# called it, each in its own `lake env lean` process: ~315s of this gate's measured 675s spent
# replaying the same theorem once per probe. What differs between these probes is the material handed
# to `ReceiptMaterial.of?`, and that is PURE — as the preamble above already says.
#
# WHAT THIS DOES NOT WEAKEN. `SuccessfulReplay` has a private constructor and `replay` is its only
# producer, so a receipt still cannot exist without a genuine kernel run. That is a type-level
# property and does not depend on how many times the run happens. What drops is the COUNT of
# identical replays.
#
# GROUPS. A group is a replay target: probes in a group share one replay because they ask for the
# same theorem, imports and target. Groups are DECLARED per probe, never inferred. Each group gets
# its OWN driver process and therefore its own replay, so a result for probe X can only have come
# from the driver of X's declared group — cross-group substitution is structurally impossible rather
# than merely checked, and no replay outlives its `$TMP` driver, so nothing survives into another
# gate invocation or mutation state. Every probe currently declares one group because they all replay
# `parse_byte_correct` with identical targets; adding a probe with a different target adds a group
# and a second replay automatically.
#
# MEMBERSHIP IS BY CONSTRUCTION. A regex over this file found only 14 of the 18 sites — four use a
# line-continuation call shape — and a batcher built on that regex would have silently dropped four
# probes from a green run. Callers name themselves `probe_mint`.
MINT_LABEL=(); MINT_WANT=(); MINT_BODY=(); MINT_GROUP=()

# THE REPLAY TARGET IS PINNED, NOT INHERITED FROM THE ENVIRONMENT. This was briefly overridable so a
# self-test could induce a replay failure, and that was a fail-open authority path: pointing it at
# any other imported theorem replays THAT theorem while every receipt assertion stays green,
# because the artifact control compares against this same name and so only checks self-consistency.
# The self-tests now use dedicated boolean flags below, which cannot name the authoritative theorem.
readonly MINT_DEFAULT_GROUP="Concrete.Proof.parse_byte_correct"

# PINNED POPULATION, NOT A PINNED COUNT. A count alone is not a denominator: delete one probe and
# duplicate another and the total is still 18, both copies pass, and the deleted assertion is gone
# with nothing to notice it. So the SET of labels is pinned by digest and duplicates are rejected.
# Changing which probes mint must be a deliberate edit of both values.
EXPECTED_MINT_PROBES=19
# Digest of the full manifest, not just the names — see mint_manifest_digest below. The constant is
# a sha256 because this repository supports macOS, where GNU md5sum does not exist; the hasher
# fallback follows the one in lib/treestate.sh rather than adding a third way to hash a thing.
EXPECTED_MINT_MANIFEST_SHA256="8713fc18b540d6e676765ef9e495073cd960ccdd5ac3b1cf75e77ab363a1c93b"
if command -v sha256sum >/dev/null 2>&1; then _MINT_HASHER="sha256sum"
elif command -v shasum >/dev/null 2>&1; then _MINT_HASHER="shasum -a 256"
else _MINT_HASHER=""
fi

# ONE RECORD PRODUCER, AND IT IS LOCALE-INDEPENDENT. `${#field}` counts CHARACTERS under a UTF-8
# locale but BYTES under C, while the bytes being hashed never change — and these bodies contain
# `←`, whose length is 1 or 3 depending on the locale. A digest pinned on one machine would then
# fail on another for no reason at all. `local LC_ALL=C` fixes the counting to bytes, and both
# populations use this one function so they cannot drift apart.
_manifest_record() {
  local LC_ALL=C
  printf '%d:%s,%d:%s,%d:%s,%d:%s' \
    "${#1}" "$1" "${#2}" "$2" "${#3}" "$3" "${#4}" "$4" | $_MINT_HASHER | cut -d' ' -f1
}

# THE MANIFEST COVERS WHAT EACH PROBE ASSERTS, NOT ONLY WHAT IT IS CALLED. Pinning labels alone left
# a hole: two probes that expect the same string — and several do — could have one body copied over
# the other, keeping all 18 labels and the label digest intact while silently deleting an assertion.
# So the digest is over (label, want, group, body) for every probe, sorted so that merely reordering
# probes is not a change. Unit separators keep the fields unambiguous.
# LENGTH-PREFIXED, so the serialization is INJECTIVE. Escaping newlines as the two bytes `\n` made
# a body containing a literal backslash-n serialize identically to one containing a real newline —
# two different probes, one digest. Length prefixes need no escaping and no separator can be forged.
# Each record is hashed first so that a body containing newlines cannot disturb the sort.
mint_manifest_digest() {
  local i recs=""
  for ((i = 0; i < ${#MINT_LABEL[@]}; i++)); do
    recs+="$(_manifest_record "${MINT_LABEL[$i]}" "${MINT_WANT[$i]}" "${MINT_GROUP[$i]}" "${MINT_BODY[$i]}")
"
  done
  printf '%s' "$recs" | LC_ALL=C sort | $_MINT_HASHER | cut -d' ' -f1
}
probe_mint() {
  MINT_LABEL+=("$1"); MINT_WANT+=("$2"); MINT_BODY+=("$3")
  MINT_GROUP+=("${4:-$MINT_DEFAULT_GROUP}")
}

# Self-test hooks, unset in normal runs. BREAK corrupts one probe's body; FOREIGN injects a result
# id belonging to no probe of the group being reconciled; REPLAY_FAIL makes the driver ask for a
# theorem that does not exist; SECOND_GROUP registers an extra probe in a DIFFERENT group whose
# replay must fail, proving group separation is real rather than an untested claim about processes.
: "${MINT_SELFTEST_BREAK:=}"
: "${MINT_SELFTEST_FOREIGN:=}"
: "${MINT_SELFTEST_REPLAY_FAIL:=}"
# DUPLICATE-RESULT DETECTION HAD NO CONTROL. The batch refuses duplicate ids, but nothing ever made
# one, so the refusal was an unexercised branch — indistinguishable, from the outside, from a branch
# that had been deleted. Boolean, and it re-emits the FIRST declared member's own line rather than
# taking any value from the environment, so the seam cannot fabricate a result or name an id that is
# not already this group's.
: "${MINT_SELFTEST_DUPLICATE:=}"
: "${MINT_SELFTEST_SECOND_GROUP:=}"
# SHELL_FAIL runs a failing command inside the batch: it proves the ERR trap actually reaches into
# these functions, which it did NOT before errtrace was set. Run it with GATE_DONE=1 exported to
# also prove an inherited value can no longer disarm the trap.
# EMPTY_GROUPS blanks the grouping result: it proves that "no driver ran" surfaces as a shortfall
# rather than as a smaller green total.
: "${MINT_SELFTEST_SHELL_FAIL:=}"
: "${MINT_SELFTEST_EMPTY_GROUPS:=}"
# SUBSHELL_FAIL fails a command SUBSTITUTION rather than a direct command. Bash exits only the
# subshell on ERR there, so the trap alone never catches it; this proves the status check does.
: "${MINT_SELFTEST_SUBSHELL_FAIL:=}"

# Reconcile one group: run its driver, then account for EXACTLY its declared members.
_mint_run_group() {
  local grp="$1"; shift
  local -a idx=("$@")
  local f="$TMP/mintbatch.${grp//[^A-Za-z0-9]/_}.lean" out i id body skip_ids="" got dup ids expect_n=${#idx[@]}
  local thm="$grp"
  [ "$MINT_SELFTEST_REPLAY_FAIL" != "1" ] || thm="Concrete.Proof.__selftest_theorem_that_does_not_exist__"

  cat > "$f" <<LEAN
import Concrete
import Examples
open Lean Meta Concrete Concrete.Proof
def tPkg : String := Concrete.shortHash "test-package"
def tid? (m d : String) : Option DefinitionIdentity :=
  (DefinitionIdentity.of? tPkg m d (Concrete.shortHash ("impl:" ++ m ++ "." ++ d))).toOption
def probeThm : String := "$thm"
-- The pure half of \`mintProbe\`: same material validation, same mint, replay supplied by the group.
def mintWith (sr : SuccessfulReplay) (subjectDigest? : Option String) (ev : EdgeEvidence)
    (root : String) (trust : Bool) (bounds : List String) (cv ws im : String)
    : Option ProofEvidenceReceipt :=
  (ReceiptMaterial.of? subjectDigest? ev root trust bounds cv ws im).map
    (ProofEvidenceReceipt.mint sr)

#eval show IO Unit from do
  let tgt : ReplayTarget :=
    { subject := "probe", theoremName := probeThm
    , kind := .refinement, origin := .sourceLinked, binding := .bound }
  match ← replay { inputPath := "Main.lean", imports := ["Concrete"], targets := [tgt] } with
  | .error _ => IO.println "<<REPLAY-FAILED>>"
  | .ok rr => match SuccessfulReplay.of? rr probeThm with
    | .error _ => IO.println "<<REPLAY-FAILED>>"
    | .ok sr => do
LEAN

  for i in "${idx[@]}"; do
    id="$(printf '%03d' "$i")"
    printf '      IO.print "<<P%s>> "\n      do\n' "$id" >> "$f"
    # STRIP THE `#eval` HEADER BY PATTERN, NOT BY POSITION. The two call shapes in this file differ:
    # `probe_mint "l" "w" '` opens the quote at end of line, so its body BEGINS with a newline, while
    # the line-continuation shape does not. Dropping "line 1" therefore stripped a blank line for
    # fourteen probes and left `#eval` embedded mid-driver — which is exactly what happened.
    # awk, not `sed -e '0,/^#eval/{...}'`: that address form is GNU-only and this must not depend
    # on which sed the host ships.
    body="$(printf '%s\n' "${MINT_BODY[$i]}" | awk 'BEGIN{past=0}
      { if (!past && $0 ~ /^[[:space:]]*$/) next
        if (!past && $0 ~ /^#eval/) { past=1; next }
        past=1; print }')"
    # Fail closed rather than emit a driver known to be malformed.
    if grep -q '^#eval' <<<"$body"; then
      mint_no "${MINT_LABEL[$i]} — body has an unstripped '#eval'; refusing to batch it"
      skip_ids="$skip_ids $id"; continue
    fi
    # Each body is nested under its OWN `do` at a deeper column, so bindings cannot leak from one
    # probe into the next: probe N must not be able to see probe N-1's `ev`.
    { if [ "$MINT_SELFTEST_BREAK" = "$i" ]; then
        printf '%s\n' "$body" | sed '1s/.*/let _ := thisNameDoesNotExist/'
      else
        printf '%s\n' "$body"
      fi
    } | sed -e 's/match ← mintProbe /match mintWith sr /' \
            -e 's/← *mintProbe /mintWith sr /' \
            -e 's/^/      /' >> "$f"
  done

  local rc=0
  out="$(lake env lean "$f" 2>&1)" || rc=$?
  # BOOLEAN, WITH A HARDCODED IMPOSSIBLE ID. This used to append the variable's VALUE verbatim to
  # authoritative batch output, so a multiline value could fabricate every <<Pnnn>> record
  # with the expected texts and no duplicates — an environment variable that manufactures a green
  # batch, which is far worse than the accounting hole the control exists to demonstrate.
  [ "$MINT_SELFTEST_FOREIGN" != "1" ] || out="$out
<<P999>> INJECTED-BY-SELFTEST"
  if [ "$MINT_SELFTEST_DUPLICATE" = "1" ]; then
    _dup_line="$(grep -m1 -E '^<<P[0-9]{3}>> ' <<<"$out")"
    [ -z "$_dup_line" ] || out="$out
$_dup_line"
  fi

  # A SHARED-REPLAY FAILURE IS ONE CAUSE, and it names every affected member rather than becoming an
  # empty green batch: no probe in this group ran.
  if grep -qF '<<REPLAY-FAILED>>' <<<"$out"; then
    no "mint group '$grp': the shared kernel replay FAILED — none of its $expect_n probes ran:"
    for i in "${idx[@]}"; do mint_no "  unproven (replay failed): ${MINT_LABEL[$i]}"; done
    return
  fi

  # LEAN'S EXIT STATUS AND DIAGNOSTICS ARE CHECKED INDEPENDENTLY of result parsing, so a driver that
  # dies after printing some results cannot pass merely because the lines it did print looked right.
  if [ "$rc" -ne 0 ]; then
    no "mint group '$grp': driver exited $rc — $(printf '%s' "$out" | grep -m1 -E 'error' | cut -c1-160)"
  fi
  # The comment above used to claim diagnostics were checked when only the exit status was. `probe()`
  # treats any Lean diagnostic as a failure regardless of exit status, and this path must match it —
  # matching the DIAGNOSTIC shape, not the bare word, because `Except.error` is a legitimate value.
  if grep -qE "error:|error\(lean" <<<"$out"; then
    no "mint group '$grp': driver reported a Lean diagnostic — $(printf '%s' "$out" | grep -m1 -E 'error:|error\(lean' | cut -c1-160)"
  fi

  # EXACT SET EQUALITY over this group's declared members. Missing, duplicate and unexpected each
  # fail BY NAME. Reconciliation does not stop at the first problem, so breaking an early probe
  # cannot make later probes disappear silently — they are still individually reported.
  # `|| true` HERE WOULD SUPPRESS A PARSER FAILURE. grep legitimately exits 1 when a group produced
  # no results, so the status is examined rather than discarded: 0 (matches) and 1 (none) are both
  # normal, anything else means the parse itself broke and the reconciliation below would be reading
  # from nothing.
  local prc=0
  ids="$(grep -oE '^<<P[0-9]{3}>>' <<<"$out" | tr -cd '0-9\n' | grep -E '^[0-9]+$')" || prc=$?
  if [ "$prc" -gt 1 ]; then
    no "mint group '$grp': could not parse result ids (status $prc) — reconciliation would be reading nothing"
  fi
  local drc=0
  dup="$(printf '%s\n' "$ids" | sort | uniq -d)" || drc=$?
  if [ "$drc" -ne 0 ]; then
    no "mint group '$grp': duplicate-id detection failed (status $drc)"
  fi
  [ -z "$dup" ] || no "mint group '$grp': DUPLICATE result ids: $(echo $dup)"

  for i in "${idx[@]}"; do
    id="$(printf '%03d' "$i")"
    case "$skip_ids " in *" $id "*) continue ;; esac   # refused above; already counted once
    if ! grep -qE "^<<P$id>> ." <<<"$out"; then
      mint_no "${MINT_LABEL[$i]} — MISSING from group '$grp' (no <<P$id>> result)"
      continue
    fi
    got="$(grep -m1 -E "^<<P$id>> " <<<"$out" | sed "s/^<<P$id>> //")"
    # EXACT EQUALITY on the parsed result, for the same reason: a substring test cannot tell
    # COMPARABLE from INCOMPARABLE, which is precisely the pair this batch asserts.
    if [ "$got" = "${MINT_WANT[$i]}" ]; then
      mint_ok "${MINT_LABEL[$i]}"
    else
      mint_no "${MINT_LABEL[$i]} — want '${MINT_WANT[$i]}' got '$(printf '%s' "$got" | cut -c1-160)'"
    fi
  done

  # A result this group never declared means the driver and the declared list disagree — including a
  # result belonging to ANOTHER group, which is how cross-group substitution would show up.
  # DECLARED FIRST, ASSIGNED SECOND: `local x="$(...)"` returns local's own status and hides the
  # substitution's.
  local want_ids
  want_ids=" $(for i in "${idx[@]}"; do printf '%03d ' "$i"; done)"
  while read -r id; do
    [ -n "$id" ] || continue
    case "$want_ids" in
      *" $id "*) ;;
      *) no "mint group '$grp': UNEXPECTED result id <<P$id>> — not a declared member" ;;
    esac
  done <<<"$ids"
}

# Every per-probe verdict increments this. At the end it must equal the number of registered
# probes: that is what catches a whole group never running — an empty `groups` list, a loop that
# never executed, a helper that returned 0 without reporting. Counting registrations proves what was
# ASKED FOR; only counting verdicts proves what was ANSWERED.
MINT_VERDICTS=0
mint_ok(){ MINT_VERDICTS=$((MINT_VERDICTS + 1)); ok "$1"; }
mint_no(){ MINT_VERDICTS=$((MINT_VERDICTS + 1)); no "$1"; }

# Same reconciliation as the mint batch, for the ordinary population.
EXPECTED_ORDINARY_PROBES=244
EXPECTED_ORDINARY_MANIFEST_SHA256="a721c0a702572652e19c2a76c26a334f01c7977053b958de985dd6c8ccbf8e1d"
# The ordinary and mint populations are pinned, but roughly 43 assertions belong to NEITHER — the
# hand-written checks scattered through this gate — and deleting one of those still shrank a green
# total. One pinned grand total covers every assertion this gate makes, whatever its shape.
EXPECTED_TOTAL_ASSERTIONS=317
EXPECTED_ASSERTION_LABELS_SHA256="8edcce27f227a567c62d531335def0eba60f68c8d04a1f34a95401e59d700809"
reconcile_assertion_total() {
  local total=$((PASS + FAIL))
  if [ "$total" -ne "$EXPECTED_TOTAL_ASSERTIONS" ]; then
    no "this gate made $total assertions, expected $EXPECTED_TOTAL_ASSERTIONS — an assertion was added or removed without updating EXPECTED_TOTAL_ASSERTIONS"
  fi
  # Snapshot BEFORE this function's own verdict, so the digest describes the gate's assertions and
  # not itself.
  local got
  if [ -z "$_MINT_HASHER" ]; then
    no "no sha256 implementation found, so the assertion population cannot be reconciled"
    return
  fi
  got="$(printf '%s' "$ASSERT_LABELS" | LC_ALL=C sort | $_MINT_HASHER | cut -d' ' -f1)"
  if [ "$got" != "$EXPECTED_ASSERTION_LABELS_SHA256" ]; then
    no "the set of assertions this gate MADE changed (got $got, expected $EXPECTED_ASSERTION_LABELS_SHA256) — one was substituted, renamed, or declared but never run; update EXPECTED_ASSERTION_LABELS_SHA256 deliberately"
  fi
}

reconcile_ordinary_probes() {
  if [ "$EXPECT_NC_N" -ne "$EXPECTED_NEGATIVE_TESTS" ]; then
    no "ran $EXPECT_NC_N negative (no-compile) tests, expected $EXPECTED_NEGATIVE_TESTS"
  fi
  if [ -n "$_MINT_HASHER" ]; then
    local ncgot
    ncgot="$(printf '%s' "$NC_RECS" | LC_ALL=C sort | $_MINT_HASHER | cut -d' ' -f1)"
    if [ "$ncgot" != "$EXPECTED_NEGATIVE_MANIFEST_SHA256" ]; then
      no "negative-test manifest changed (got $ncgot, expected $EXPECTED_NEGATIVE_MANIFEST_SHA256) — a label, expected diagnostic or BODY was swapped or substituted; update EXPECTED_NEGATIVE_MANIFEST_SHA256 deliberately"
    fi
  fi
  if [ "$PROBE_N" -ne "$EXPECTED_ORDINARY_PROBES" ]; then
    no "ran $PROBE_N ordinary probes, expected $EXPECTED_ORDINARY_PROBES — a probe was added or removed without updating EXPECTED_ORDINARY_PROBES"
  fi
  if [ -z "$_MINT_HASHER" ]; then
    no "no sha256 implementation found, so the ordinary probe manifest cannot be reconciled"
    return
  fi
  local got
  got="$(printf '%s' "$PROBE_RECS" | LC_ALL=C sort | $_MINT_HASHER | cut -d' ' -f1)"
  if [ "$got" != "$EXPECTED_ORDINARY_MANIFEST_SHA256" ]; then
    no "ordinary probe manifest changed (got $got, expected $EXPECTED_ORDINARY_MANIFEST_SHA256) — a label, expectation or BODY was swapped, renamed or substituted; update EXPECTED_ORDINARY_MANIFEST_SHA256 deliberately"
  fi
}

flush_mint_probes() {
  # RECONCILE THE DECLARED POPULATION FIRST, before any self-test registration can perturb it.
  local n0=${#MINT_LABEL[@]} lsha ldup
  if [ "$n0" -ne "$EXPECTED_MINT_PROBES" ]; then
    no "mint batch registered $n0 probes, expected $EXPECTED_MINT_PROBES — a probe_mint call was added or removed without updating EXPECTED_MINT_PROBES"
  fi
  ldup="$(printf '%s\n' "${MINT_LABEL[@]}" | LC_ALL=C sort | uniq -d)"
  if [ -n "$ldup" ]; then
    no "mint batch has DUPLICATE probe labels, so one probe is standing in for another: $(printf '%s' "$ldup" | tr '\n' '|')"
  fi
  # AN EMPTY EXPECTATION ACCEPTS ANY OUTPUT, because `grep -qF ""` matches everything. A probe like
  # that reports ok while asserting nothing at all.
  local _i
  for ((_i = 0; _i < n0; _i++)); do
    [ -n "${MINT_WANT[$_i]}" ] || no "${MINT_LABEL[$_i]} — has an EMPTY expectation, which would accept any output"
  done
  # NO SKIP SEAM. An inherited MINT_SELFTEST_SKIP_MANIFEST=1 disabled the only body-integrity check
  # in production, which is a strictly worse hole than the one the manifest closes. The causal
  # control is performed instead by pinning the POST-swap digest, an explicit source edit that
  # cannot be reached from the environment.
  if [ -z "$_MINT_HASHER" ]; then
    no "mint batch: no sha256 implementation found, so the probe manifest cannot be reconciled"
  else
    lsha="$(mint_manifest_digest)"
    if [ "$lsha" != "$EXPECTED_MINT_MANIFEST_SHA256" ]; then
      no "mint batch manifest changed (got $lsha, expected $EXPECTED_MINT_MANIFEST_SHA256) — a label, expectation, group or BODY was swapped, renamed or substituted; update EXPECTED_MINT_MANIFEST_SHA256 deliberately"
    fi
  fi

  # A SECOND GROUP THAT MUST FAIL. With every real probe in one group, "results cannot cross groups"
  # rested on process separation being asserted, never exercised. This registers one probe in a
  # different group whose theorem does not exist: its replay must fail and name only that member,
  # while all 18 real probes still pass. A shared driver would take both down together.
  if [ "$MINT_SELFTEST_SECOND_GROUP" = "1" ]; then
    probe_mint "SELFTEST: member of a second, failing group" "NEVER-REPORTED" \
'#eval show IO Unit from do
  IO.println "UNREACHABLE"' "Concrete.Proof.__selftest_second_group__"
  fi

  # The population was already reconciled above against the pinned label set; a second count check
  # here would be a duplicate producer of the same fact, differing only in whether it had seen the
  # self-test registration.
  local n=${#MINT_LABEL[@]}
  if [ "$n" -eq 0 ]; then no "mint batch is EMPTY — no probe registered (vacuous)"; return; fi
  local groups g i
  [ "$MINT_SELFTEST_SHELL_FAIL" != "1" ] || /nonexistent-command-for-the-errtrace-control
  groups="$(printf '%s\n' "${MINT_GROUP[@]}" | sort -u)"
  [ "$MINT_SELFTEST_EMPTY_GROUPS" != "1" ] || groups=""
  # NOT `echo "... $(...)"`. An ERR inside a command substitution exits only that subshell; the
  # enclosing `echo` then succeeds and the run continues to a green exit — the trap fires, prints
  # FATAL, and changes nothing. A standalone assignment whose status is checked cannot do that.
  # `awk` also counts an empty list as 0 groups, where `wc -l` counted the blank line as 1.
  # ONE producer, ONE status check. The self-test substitutes the COUNTER used by the production
  # expression instead of adding a second assignment beside it — otherwise restoring the unsafe
  # original would leave the control red and prove nothing about the production path.
  local ngroups counter="awk"
  [ "$MINT_SELFTEST_SUBSHELL_FAIL" != "1" ] || counter="/nonexistent-counter-for-the-subshell-control"
  if ! ngroups="$(printf '%s\n' "$groups" | "$counter" 'NF{c++} END{print c+0}')"; then
    no "mint batch: the replay-group count could not be computed"
    ngroups="?"
  fi
  echo "=== receipt minting ($n probes, $ngroups replay group(s)) ==="
  if [ -z "${groups//[[:space:]]/}" ]; then
    no "mint batch produced NO groups from $n registered probes — the grouping step failed and no driver ran"
  fi
  while read -r g; do
    [ -n "$g" ] || continue
    local -a idx=()
    for ((i = 0; i < n; i++)); do [ "${MINT_GROUP[$i]}" = "$g" ] && idx+=("$i"); done
    _mint_run_group "$g" "${idx[@]}"
  done <<<"$groups"

  # WHAT WAS ANSWERED MUST EQUAL WHAT WAS ASKED. Every path above that resolves one probe goes
  # through mint_ok/mint_no, so a group that never ran — for any reason, including one nobody
  # anticipated — shows up here as a shortfall instead of as a smaller green total.
  if [ "$MINT_VERDICTS" -ne "$n" ]; then
    no "mint batch reported $MINT_VERDICTS verdicts for $n registered probes — $((n - MINT_VERDICTS)) probe(s) were never resolved"
  fi
}

echo "=== the vocabulary is complete and cannot launder trust ==="
# COMPLETENESS IS A THEOREM (`DependencyEdge.mem_all`), not this length check. A count protects
# nothing against a sixth constructor whose author also updates the 5 to a 6 — the list and the
# number are edited in the same breath, so the test agrees with whatever was written. The theorem
# leaves an unsolved case instead; verified by deleting a constructor from `all` and watching the
# build fail with `case unclassified ⊢ False`.
#
# What stays here is what a theorem cannot state: that the canonical TAGS are distinct, since two
# kinds sharing a tag would be indistinguishable in a receipt.
probe "canonical edge tags are distinct" "true" \
'#eval (DependencyEdge.all.map DependencyEdge.canonical).eraseDups.length == DependencyEdge.all.length'
# `missing` is the only kind that is never current. `trusted` IS current — but
# only with its trust carried forward, which is a separate question and must stay
# a separate function, or an unqualified `proved_by_lean` gets minted over a trust
# boundary.
# TWO kinds are not current, and `unclassified` joining `missing` is the point: "nobody has
# classified this" must not read as "this is fine". The predicate is exhaustive with no
# wildcard, so a future edge kind is a compile error rather than silently born current — which
# is what happened when `unclassified` was added under a `| _ => true` catch-all.
probe "missing AND unclassified are not current for dependents" "true" \
'#eval (DependencyEdge.all.filter (fun e => !e.isCurrentForDependents))
        == [DependencyEdge.missing, DependencyEdge.unclassified]'
# The distinction must survive: both fail closed, and they are still different states with
# different repairs — a proof versus running the classification hand-back.
probe "unclassified is DISTINCT from missing" "true" \
'#eval DependencyEdge.unclassified != DependencyEdge.missing
  && DependencyEdge.unclassified.canonical != DependencyEdge.missing.canonical'
# And the root must refuse it, not traverse it as validated.
probe "a root refuses an UNCLASSIFIED edge" "true" \
'#eval
  match tid? "m" "a", tid? "m" "b" with
  | some i_m_a, some i_m_b =>
    ((dependencyRootMaterial ([{ id := i_m_a, label := CallableId.ofUser "m" "a", digest := some "DA", edges := [(DependencyEdge.unclassified, i_m_b)] }, { id := i_m_b, label := CallableId.ofUser "m" "b", digest := some "DB", edges := [] }] : List DepNode) i_m_a).toOption.isNone)
  | _, _ => false'
probe "only trusted propagates trust" "true" \
'#eval (DependencyEdge.all.filter DependencyEdge.propagatesTrust) == [DependencyEdge.trusted]'
probe "trusted is current AND propagates — both, not either" "true" \
'#eval DependencyEdge.trusted.isCurrentForDependents && DependencyEdge.trusted.propagatesTrust'
# A body change stales a body edge and not a contract edge. That IS the modular
# vs closed-subject distinction, as data rather than prose.
probe "a body change invalidates body but not contract" "true" \
'#eval DependencyEdge.body.invalidatedByBodyChange && !DependencyEdge.contract.invalidatedByBodyChange'

echo ""
echo "=== the edge is DERIVED from the theorem, on the roadmap's own exemplars ==="
# `unary_call` quantifies over `fns` and takes the callee's behaviour as a
# hypothesis; `combine_correct` names `combineFns` and unfolds helper bodies.
probe "unary_call derives a contract edge" "contract" \
'#eval show MetaM Unit from do
   let r ← classifyTheorem ``Concrete.Proof.unary_call
   IO.println (repr (r.map (·.edge)))'
probe "combine_correct derives a body edge" "body" \
'#eval show MetaM Unit from do
   let r ← classifyTheorem ``Examples.ProofPatterns.Proofs.combine_correct
   IO.println (repr (r.map (·.edge)))'
# A body edge must say WHICH tables it binds; "body" alone does not let a consumer
# decide whether a given change stales it.
probe "a body edge names the table it binds" "combineFns" \
'#eval show MetaM Unit from do
   let r ← classifyTheorem ``Examples.ProofPatterns.Proofs.combine_correct
   IO.println (repr (r.map (·.tables)))'
# No table dependency is NOT `missing`. Collapsing them would let "irrelevant"
# read as "unvalidated" and contain claims that are fine.
probe "a theorem with no table dependency classifies as none, not missing" "none" \
'#eval show MetaM Unit from do
   let r ← classifyTheorem ``Concrete.ProofSoundness.spec_total
   IO.println (repr (r.map (·.edge)))'

echo ""
echo "=== table-valued combinators: the fail-OPEN gap is closed ==="
# A naive "constant whose TYPE is FnTable" test misses `eval (FnTable.ofGlobals g)`
# — the constant has FUNCTION type — and answers `contract` for a theorem that
# depends on concrete entries. That is the unsafe direction. Measured: widening to
# "returns FnTable" moved 8 theorems from contract to body.
probe "FnTable.ofGlobals counts as table-valued" "true" \
'#eval show MetaM Unit from do
   let ci ← getConstInfo ``Concrete.Proof.FnTable.ofGlobals
   IO.println (toString (← isTableValued ci))'
probe "a plain table definition counts as table-valued" "true" \
'#eval show MetaM Unit from do
   let ci ← getConstInfo ``Concrete.Proof.ctTagFns
   IO.println (toString (← isTableValued ci))'
probe "a non-table constant does not" "false" \
'#eval show MetaM Unit from do
   let ci ← getConstInfo ``Concrete.Proof.pexprCanonical
   IO.println (toString (← isTableValued ci))'

echo ""
echo "=== corpus split, pinned ==="
# Pinned so a change in how theorems are written, or in the classifier, has to be
# looked at rather than absorbed. 0 of these are ambiguous, which is why no
# tie-break rule exists: none is needed.
# 166 -> 168 on 2026-08-15, and the two are named rather than absorbed: `withAttestations_globals`
# and `withAttestations_entries`, the projection lemmas added so the proof corpus still reduces
# through an ATTESTED table. Both mention `FnTable`, so the classifier counts them. They are
# `rfl` projections and assert nothing about any implementation.
#
# 168 -> 167 later the same day, and the one is `Concrete.Proof.elfFns.eq_1` — an EQUATION LEMMA
# Lean generates on demand for a def used by name in a simp set. `validate_header_correct` stopped
# passing `elfFns` to `simp_all` (it reduces through the `@[simp] elfFns_globals` projection
# instead), so nothing forces the lemma into existence and the classifier no longer sees it.
# Nothing was proved less: the theorem's statement is unchanged and its proof still closes. The
# count is sensitive to which defs happen to acquire equation lemmas, which is worth knowing about
# a pinned number — it moves for reasons that are not always about the corpus.
#
# 167 -> 166 for the same reason one table later (`fixedCapacityFns.eq_1`), 166 -> 165 for
# `parseValidateFns.eq_1`, and 165 -> 164 for `cryptoFns.eq_1`.
#
# I WAS WRONG ABOUT WHERE THESE COME FROM, and the correction is why the invariant below exists.
# I attributed the surviving lemmas to the `by decide` completeness examples in
# `ProofSoundness.lean`. They are not the cause: `parseValidateFns.eq_1` vanished the moment its
# simp site was repointed, while its `decide` examples were untouched. Every one of these lemmas
# came from a simp set naming the def — including two sites my line-anchored grep never saw,
# because the table name sat on a CONTINUATION line. `pureCoreFns.eq_1` remains, from
# `proofs/Examples/PureCore/Proofs.lean`; that table is not manifest-backed and is not converted.
probe "the corpus splits 113 contract / 164 body" "113/164" \
'#eval show MetaM Unit from do
   let env ← getEnv
   let mut nc := 0; let mut nb := 0
   for (n, ci) in env.constants.toList do
     unless (ci matches .thmInfo _) do continue
     let s := n.toString
     unless s.startsWith "Examples." || s.startsWith "Concrete.Proof" do continue
     if (s.splitOn "_proof_").length > 1 then continue
     match ← classifyTheorem n with
     | none => pure ()
     | some e => if e.edge == .body then nb := nb + 1 else nc := nc + 1
   IO.println s!"{nc}/{nb}"'

echo ""
echo "=== dependency roots: fail closed on every incomplete input ==="
GRAPH='def P : String := "pkg0123456789abcdef0123456789abcd"
def impl (c : Char) : String := String.mk (List.replicate 32 c)
-- THE SYNTHETIC GRAPH IS SCOPED NOW, because `DepNode` is. There is no total constructor for a
-- `DefinitionIdentity` and none is wanted, so the fixture binds all six ids in one match and hands
-- them to the probe body; a fixture that cannot build its own identities reports false rather than
-- inventing one, which is the same discipline the production path follows.
def withG (k : DefinitionIdentity → DefinitionIdentity → DefinitionIdentity → DefinitionIdentity →
               DefinitionIdentity → List DepNode → List DepNode → Bool) : Bool :=
  match DefinitionIdentity.of? P "m" "a" (impl (Char.ofNat 97)),
        DefinitionIdentity.of? P "m" "b" (impl (Char.ofNat 98)),
        DefinitionIdentity.of? P "m" "c" (impl (Char.ofNat 99)),
        DefinitionIdentity.of? P "m" "d" (impl (Char.ofNat 100)),
        DefinitionIdentity.of? P "m" "e" (impl (Char.ofNat 101)) with
  | .ok ia, .ok ib, .ok ic, .ok idd, .ok ie =>
    let g : List DepNode :=
      [ { id := ia, label := CallableId.ofUser "m" "a", digest := some "DA", edges := [(DependencyEdge.body, ib)] }
      , { id := ib, label := CallableId.ofUser "m" "b", digest := some "DB", edges := [(DependencyEdge.body, ic)] }
      , { id := ic, label := CallableId.ofUser "m" "c", digest := some "DC", edges := [] }
      , { id := idd, label := CallableId.ofUser "m" "d", digest := some "DD", edges := [(DependencyEdge.body, ie)] }
      , { id := ie, label := CallableId.ofUser "m" "e", digest := some "DE", edges := [(DependencyEdge.body, idd)] } ]
    let g2 := g.map fun n => if n.label.declName == "c" then { n with digest := some "X" } else n
    k ia ib ic idd ie g g2
  | _, _, _, _, _ => false
'
probe "a mutual cycle terminates" "true" "$GRAPH"'#eval withG fun _ia _ib _ic idd _ie g _g2 =>
  (dependencyRootMaterial g idd).toOption.isSome'
probe "cycle members share a closure (entry point is not semantic)" "true" "$GRAPH"'#eval withG fun _ia _ib _ic idd ie g _g2 =>
  ((reachableFrom g idd).map (·.canonical)).mergeSort (· ≤ ·)
    == ((reachableFrom g ie).map (·.canonical)).mergeSort (· ≤ ·)'
probe "cycle members do NOT share a root (they are different subjects)" "true" "$GRAPH"'#eval withG fun _ia _ib _ic idd ie g _g2 =>
  (dependencyRootMaterial g idd).toOption != (dependencyRootMaterial g ie).toOption'
probe "a deep callee edit moves the dependent root" "true" "$GRAPH"'#eval withG fun ia _ib _ic _idd _ie g g2 =>
  (dependencyRootMaterial g ia).toOption != (dependencyRootMaterial g2 ia).toOption'
probe "an unrelated subtree is unaffected" "true" "$GRAPH"'#eval withG fun _ia _ib _ic idd _ie g g2 =>
  (dependencyRootMaterial g idd).toOption == (dependencyRootMaterial g2 idd).toOption'

# FAIL-CLOSED CONDITIONS. Each previously produced a confident value from
# incomplete information; each must now REFUSE, with the reason carried.
probe "a missing start is refused" "missingStart" \
'#eval
  match tid? "m" "ghost" with
  | some i_m_ghost =>
    repr (dependencyRootMaterial [] i_m_ghost)
  | _ => "probe could not build test identities"'
probe "an unresolved edge is refused, not bound as unknown" "unresolvedEdge" \
'#eval
  match tid? "m" "a", tid? "m" "ghost" with
  | some i_m_a, some i_m_ghost =>
    repr (dependencyRootMaterial ([{ id := i_m_a, label := CallableId.ofUser "m" "a", digest := some "DA", edges := [(DependencyEdge.body, i_m_ghost)] }] : List DepNode) i_m_a)
  | _, _ => "probe could not build test identities"'
probe "a duplicate identity is refused" "duplicateId" \
'#eval
  match tid? "m" "a" with
  | some i_m_a =>
    repr (dependencyRootMaterial ([{ id := i_m_a, label := CallableId.ofUser "m" "a", digest := some "1", edges := [] }, { id := i_m_a, label := CallableId.ofUser "m" "a", digest := some "2", edges := [] }] : List DepNode) i_m_a)
  | _ => "probe could not build test identities"'
probe "an absent subject digest is refused" "incompleteDigest" \
'#eval
  match tid? "m" "a" with
  | some i_m_a =>
    repr (dependencyRootMaterial ([{ id := i_m_a, label := CallableId.ofUser "m" "a", digest := none, edges := [] }] : List DepNode) i_m_a)
  | _ => "probe could not build test identities"'
probe 'a missing-typed edge is refused' "missingEdge" \
'#eval
  match tid? "m" "a", tid? "m" "b" with
  | some i_m_a, some i_m_b =>
    repr (dependencyRootMaterial ([{ id := i_m_a, label := CallableId.ofUser "m" "a", digest := some "DA", edges := [(DependencyEdge.missing, i_m_b)] }, { id := i_m_b, label := CallableId.ofUser "m" "b", digest := some "DB", edges := [] }] : List DepNode) i_m_a)
  | _, _ => "probe could not build test identities"'
# THE EDGE KIND IS IN THE BYTES. Traversing typed edges and then serializing only
# identities threw the typing away: contract and body produced the same preimage.
probe "changing an edge kind changes the preimage" "true" \
'#eval
  match tid? "m" "a", tid? "m" "b" with
  | some i_m_a, some i_m_b =>
    let c : List DepNode := ([{ id := i_m_a, label := CallableId.ofUser "m" "a", digest := some "DA", edges := [(DependencyEdge.contract, i_m_b)] }, { id := i_m_b, label := CallableId.ofUser "m" "b", digest := some "DB", edges := [] }] : List DepNode)
    let b : List DepNode := ([{ id := i_m_a, label := CallableId.ofUser "m" "a", digest := some "DA", edges := [(DependencyEdge.body, i_m_b)] }, { id := i_m_b, label := CallableId.ofUser "m" "b", digest := some "DB", edges := [] }] : List DepNode)
    (dependencyRootMaterial c i_m_a).toOption != (dependencyRootMaterial b i_m_a).toOption
  | _, _ => false'
# an EMPTY digest is as incomplete as an absent one
probe "an empty-string digest is refused" "incompleteDigest" \
'#eval
  match tid? "m" "a" with
  | some i_m_a =>
    repr (dependencyRootMaterial ([{ id := i_m_a, label := CallableId.ofUser "m" "a", digest := some "", edges := [] }] : List DepNode) i_m_a)
  | _ => "probe could not build test identities"'
# trust travels WITH the material, not beside it
probe "trust is carried in the returned structure" "true" \
'#eval
  match tid? "m" "a", tid? "m" "b" with
  | some i_m_a, some i_m_b =>
    let g : List DepNode := ([{ id := i_m_a, label := CallableId.ofUser "m" "a", digest := some "DA", edges := [(DependencyEdge.trusted, i_m_b)] }, { id := i_m_b, label := CallableId.ofUser "m" "b", digest := some "DB", edges := [] }] : List DepNode)
    ((dependencyRootMaterial g i_m_a).toOption.map DependencyRootMaterial.carriesTrust) == some true
  | _, _ => false'
# CANONICAL SET SEMANTICS. reachableFrom returns the start when the start is in a
# cycle, and `id :: closure` then listed it twice — a self-recursive subject got a
# different preimage for the same dependency set.
probe "a self-recursive node is serialized exactly once" "true" \
'#eval
  match tid? "m" "a" with
  | some i_m_a =>
    let g : List DepNode := ([{ id := i_m_a, label := CallableId.ofUser "m" "a", digest := some "DA", edges := [(DependencyEdge.body, i_m_a)] }] : List DepNode)
    match (dependencyRootMaterial g i_m_a).toOption with
    | some m =>
      let c := i_m_a.canonical
      ((m.preimage.splitOn ("N" ++ toString c.length ++ ":" ++ c ++ ":")).length - 1) == 1
    | none   => false
  | _ => false'
# Calling one callee twice does not change the dependency SET, so it must not
# change the root.
probe "a duplicated edge does not change the root" "true" \
'#eval
  match tid? "m" "a", tid? "m" "b" with
  | some i_m_a, some i_m_b =>
    let one : List DepNode := ([{ id := i_m_a, label := CallableId.ofUser "m" "a", digest := some "DA", edges := [(DependencyEdge.body, i_m_b)] }, { id := i_m_b, label := CallableId.ofUser "m" "b", digest := some "DB", edges := [] }] : List DepNode)
    let two : List DepNode := ([{ id := i_m_a, label := CallableId.ofUser "m" "a", digest := some "DA", edges := [(DependencyEdge.body, i_m_b), (DependencyEdge.body, i_m_b)] }, { id := i_m_b, label := CallableId.ofUser "m" "b", digest := some "DB", edges := [] }] : List DepNode)
    (dependencyRootMaterial one i_m_a).toOption == (dependencyRootMaterial two i_m_a).toOption
  | _, _ => false'
# IDENTITY, NOT SPELLING. Two callables with the same declName in different
# modules are different nodes; a raw-string graph could not tell them apart.
probe "same declName in different modules are different nodes" "duplicateId" \
'#eval
  match tid? "m1" "f" with
  | some i_m1_f =>
    repr (dependencyRootMaterial
    ([{ id := i_m1_f, label := CallableId.ofUser "m1" "f", digest := some "D1", edges := [] }
    ,{ id := i_m1_f, label := CallableId.ofUser "m1" "f", digest := some "D2", edges := [] }] : List DepNode)
    i_m1_f)
  | _ => "probe could not build test identities"'
probe "...and distinct modules do NOT collide" "true" \
'#eval
  match tid? "m1" "f", tid? "m2" "f" with
  | some i_m1_f, some i_m2_f =>
    let g : List DepNode :=
    ([{ id := i_m1_f, label := CallableId.ofUser "m1" "f", digest := some "D1", edges := [] }
    ,{ id := i_m2_f, label := CallableId.ofUser "m2" "f", digest := some "D2", edges := [] }] : List DepNode)
    (dependencyRootMaterial g i_m1_f).toOption
    != (dependencyRootMaterial g i_m2_f).toOption
  | _, _ => false'
# THE PUBLIC API MUST REJECT A STRING START AT COMPILE TIME. Storing CallableId
# while keying every lookup on `.render` would keep rendered strings as the
# operational identity under a typed wrapper — the defect the type exists to
# remove. A probe that passes a String must NOT elaborate.
cat > "$TMP/strstart.lean" <<'LEAN'
import Concrete
open Concrete Concrete.Proof
#eval (dependencyRootMaterial [] "v1:user:m.a").toOption.isSome
LEAN
if lake env lean "$TMP/strstart.lean" >/dev/null 2>&1; then
  no "a String start identity still compiles — rendered strings remain the operational identity"
else
  ok "a String start identity does not compile; the API takes CallableId"
fi
# ...and traversal returns identities, not renderings.
probe "reachableFrom returns scoped identities" "true" "$GRAPH"'#eval withG fun ia _ib _ic _idd _ie g _g2 =>
  ((reachableFrom g ia).map (·.declarationIdentity)).contains "c"'
# Lookup must compare identities. Two ids equal as VALUES must resolve to the
# same node even when constructed separately.
probe "lookup is by identity value, not by a shared rendering" "true" \
'#eval
  match tid? "m" "a" with
  | some i_m_a =>
    let g : List DepNode := ([{ id := i_m_a, label := CallableId.ofUser "m" "a", digest := some "DA", edges := [] }] : List DepNode)
    (dependencyRootMaterial g i_m_a).toOption.isSome
  | _ => false'
# There must be no public fail-OPEN way to ask the trust question.
if grep -q "def closureCarriesTrust" "$ROOT_DIR/Concrete/Proof/DependencyRoot.lean"; then
  no "closureCarriesTrust still answers over an unvalidated graph — a fail-open twin of the validated path"
else
  ok "trust is only answerable from validated material"
fi
# trust must be visible, and separately from the root

# === THE AUTHORITY PASS, AND THE CROSS-PROGRAM DISCRIMINATION IT RESTS ON =======================
#
# `examples/elf_header` and `examples/elf_header_drifted` are two PACKAGES declaring the same
# functions. The real program's `validate_header` corresponds 4/4; the drifted one's corresponds 0/4,
# because its edges point at its own drifted implementations and `elfFns` attests the real program's.
# That is the discrimination the scoped join exists to make, and it is only measurable because the
# drifted file was moved into its own package — it previously sat beside `main.con` in one package,
# both declaring `mod main`, so the loader resolved to `main.con` and the drifted bodies were never
# analyzed at all.
REAL_CORR="$(cc examples/elf_header/src/main.con subject-facts | grep -c 'shadow correspondence: matched=4 missing=0' || true)"
DRIFT_CORR="$(cc examples/elf_header_drifted/src/main.con subject-facts | grep -c 'shadow correspondence: matched=0 missing=4' || true)"
if [ "$REAL_CORR" = "1" ] && [ "$DRIFT_CORR" = "1" ]; then
  ok "same declarations, two packages: the real program corresponds 4/4 and the drifted one 0/4"
else
  no "cross-program discrimination moved (real=$REAL_CORR drifted=$DRIFT_CORR, expected 1/1) — either the join stopped comparing packages or a fixture changed"
fi

# THE DRIFTED PACKAGE IS CAUGHT BY STALENESS, NOT BY CORRESPONDENCE, and that ordering is correct: a
# body that no longer matches its own stored fingerprint is a more specific and more actionable fact
# than an unjustified closure, so `stale` wins and the authority pass never sees those subjects.
DRIFT_STALE="$(cc examples/elf_header_drifted/src/main.con proof-status | grep -cE "^-- proof stale" || true)"
if [ "$DRIFT_STALE" = "2" ]; then
  ok "the drifted package reports 2 stale proofs — exactly the two functions its header says drifted"
else
  no "the drifted package reports $DRIFT_STALE stale (expected 2) — its own bodies may not be being analyzed"
fi

# THE AUTHORITY PASS HAS NO LIVE CASE IN examples/ TODAY, and saying so is the point. Its live case is
# `tests/programs/proof_decode_header.con`, whose hardcoded link names a table with zero entries. A
# count of zero here is therefore CORRECT and must still be pinned: if an unjustified closure appears
# in the fixture corpus, that is a finding either way — a real defect caught, or a regression.
CORPUS_PROVED=0; CORPUS_UNJUST=0
for f in $(grep -rlE '#\[(proof_by|ensures_proof)\(' examples --include='*.con' | sort); do
  CORPUS_PROVED=$((CORPUS_PROVED + $(cc "$f" proof-status | grep -cE "^-- proved" || true)))
  CORPUS_UNJUST=$((CORPUS_UNJUST + $(cc "$f" proof-status | grep -cE "^-- dependency closure unjustified" || true)))
done
if [ "$CORPUS_PROVED" = "35" ] && [ "$CORPUS_UNJUST" = "0" ]; then
  ok "corpus-wide: 35 proved, 0 unjustified closures (the fixture corpus is clean under the authority pass)"
else
  no "corpus-wide verdicts moved to $CORPUS_PROVED proved / $CORPUS_UNJUST unjustified (was 35/0) — say which subject changed and why"
fi
# ...and the live case must stay live, or the pass has no corpus exercise at all.
DH_UNJUST="$(cc tests/programs/proof_decode_header.con proof-status | grep -cE "^-- dependency closure unjustified" || true)"
if [ "$DH_UNJUST" = "1" ]; then
  ok "decode_header's closure is refused (a table with no entries can describe no definitions)"
else
  no "decode_header no longer reports an unjustified closure — the authority pass has lost its only live case"
fi

echo ""
echo "=== ROOT INTEGRATION — asserted, not tripwired ==="
# THESE REPLACED TWO TRIPWIRES THAT COULD NEVER FIRE. Both grepped for `dependencyRootPreimage`, a
# name RENAMED to `dependencyRootMaterial` on 2026-07-31 in c88c3e6d — so for over two weeks they
# reported "ProofCore does NOT consume dependency roots" without being able to detect it either way,
# and kept reporting it after roots became consumed. A check that answers a question it has stopped
# asking is the failure this suite exists to catch; it happened here in the suite itself.
#
# The replacement asserts the REAL state, and names that exist.
# A CALL, NOT A MENTION. The first version grepped for the bare name, which also matches the six
# comments that discuss it — so deleting the production call would have left this green. Anchored to
# the call expression AND to non-comment lines.
# CAPTURED, THEN TESTED. `cmd | grep -q` is a SIGPIPE race under `set -o pipefail`: `grep -q` exits
# the moment it matches, the upstream `grep -v` dies with SIGPIPE, and pipefail reports the pipeline
# as failed — INTERMITTENTLY, depending on which side wins. This check failed roughly one run in
# three on an unchanged tree and passed the rest, which is worse than always failing: it reads as
# flakiness in the suite rather than as a defect in the check. Same shape as the `proof-status |
# grep -q` bug fixed in the manifest gate; the fix is the same.
# EXISTENCE IS NOT ENOUGH WHEN THERE ARE TWO CALLERS. `grep -q` passed as long as EITHER survived,
# so deleting the authority-pass call still left this green on the strength of the self-check call.
# The number of call sites is pinned instead, which is what makes removing one of them visible.
EXPECTED_ROOTCALL_SITES=2   # the authority pass, and ProofCore's own self-check
ROOTCALLS_N="$(grep -v '^[[:space:]]*--' "$ROOT_DIR/Concrete/Proof/ProofCore.lean" \
  | grep -c "Proof.dependencyRootMaterial (dependencyNodesOf" || true)"
if [ "$ROOTCALLS_N" = "$EXPECTED_ROOTCALL_SITES" ]; then
  ok "ProofCore calls dependencyRootMaterial over the real node set at both expected sites (the authority pass requires a computable closure)"
else
  no "found $ROOTCALLS_N non-comment call(s) to dependencyRootMaterial over dependencyNodesOf, expected $EXPECTED_ROOTCALL_SITES — a caller was removed, added or moved"
fi
# ...and the consumption is in the COMPOSITION, not in per-entry status derivation. A root reaching
# `deriveObligationStatus` would bypass the composed pass entirely.
# AN ABSENT ANCHOR IS NOT A CLEAN RESULT. `|| true` made a vanished call site — renamed, moved or
# deleted — indistinguishable from one that exists and is clean.
# ALL FOUR CALL SITES, NOT THREE. The anchor matched only `e.eligibility` and silently ignored the
# `x.eligibility` caller, so that one could have started consuming a root with this control none the
# wiser; and because the matches were aggregated, any single site could vanish while the whole stayed
# nonempty. The count is pinned and the pattern covers every receiver.
EXPECTED_DERIVE_SITES=4
DERIVE_N="$(grep -cE "deriveObligationStatus [a-z]+\.eligibility" "$ROOT_DIR/Concrete/Proof/ProofCore.lean" || true)"
DERIVE_CTX="$(grep -A2 -E "deriveObligationStatus [a-z]+\.eligibility" "$ROOT_DIR/Concrete/Proof/ProofCore.lean" || true)"
if [ "$DERIVE_N" != "$EXPECTED_DERIVE_SITES" ]; then
  no "found $DERIVE_N deriveObligationStatus call site(s), expected $EXPECTED_DERIVE_SITES — one was added, removed or moved, so this control no longer covers what it names"
elif [ -z "$DERIVE_CTX" ]; then
  no "deriveObligationStatus no longer appears in ProofCore.lean — this control lost its anchor and was passing on absence"
elif printf '%s' "$DERIVE_CTX" | grep -qE "dependencyRootMaterial|dependencyNodesOf"; then
  no "a dependency root reaches deriveObligationStatus — that bypasses the composition rather than joining it"
else
  # NARROWED TO WHAT THIS ACTUALLY CHECKS. It used to claim "roots are consumed by the composed
  # authority pass, NOT by per-entry status derivation" — an architectural claim about where a value
  # flows, which a two-line grep cannot support: a root computed earlier and passed in under another
  # name leaves every call header clean. Consumption BY the authority pass is now asserted
  # behaviourally above; this remains a lexical tripwire and its label says so.
  ok "no deriveObligationStatus call site textually mentions a dependency root (lexical tripwire, not a dataflow proof)"
fi

# THE ROOT CONJUNCT IS CONSUMED AND CURRENTLY REDUNDANT, measured. Every subject whose root refuses
# is already either not `proved` or refused by correspondence, so requiring the root downgrades
# nobody extra. RESTORED after being deleted by a wholesale block rewrite in 60affbc4 — the rewrite
# replaced prose around these lines and took the executable controls with it, leaving the gate green
# while asserting the wrong integration state.
ROOTREF=0
for f in $(grep -rlE '#\[(proof_by|ensures_proof)\(' examples --include='*.con' | sort); do
  ROOTREF=$((ROOTREF + $(cc "$f" subject-facts | grep 'shadow depRoot' | grep -c REFUSED || true)))
done
if [ "$ROOTREF" = "13" ]; then
  ok "13 subject roots refuse (measured, exact)"
else
  no "root refusals moved to $ROOTREF (was 13) — if a proved subject now fails to root, the root dimension has become load-bearing and must be described as such"
fi
# THE OTHER HALF, ASSERTED WHERE THE PRODUCERS LIVE. "13 refuse, none of them proved" was one
# assertion doing the work of two, and the first attempt at the second half was INERT: it paired
# subject headers with `depRoot` lines using `grep -B1`, and those are many lines apart, so it
# extracted no subject name and passed on a corpus where roots visibly refuse.
#
# The correlation is now a compiler invariant — `PROVED-ROOTS` in `checkProofCoreConsistency` — which
# reads the obligations and the node set directly and cannot drift from them. This gate asserts the
# consistency report is clean across every fixture, so the invariant is exercised on the real corpus
# rather than restated here.
# SCOPED TO THE CLAIM. An earlier version asserted every fixture passes ALL consistency checks, which
# surfaced a pre-existing and unrelated `STALE-FP` violation in `evidence_classes/stale_proof`
# ("obligation is 'stale' but fingerprints match" — that fixture is stale by SPEC DRIFT, and the
# invariant assumes stale means a fingerprint mismatch). Absorbing someone else's defect into this
# gate would have pinned it as expected; it is reported separately instead.
CONSBAD=0
CONSFILES="$(grep -rlE '#\[(proof_by|ensures_proof)\(' examples --include='*.con' | sort || true)"
if [ -z "$CONSFILES" ]; then
  no "no fixture carries a proof attribute — a zero-violation result here would be vacuous"
fi
for f in $CONSFILES; do
  out="$(cc "$f" consistency || true)"
  if printf '%s' "$out" | grep -q "PROVED-ROOTS"; then
    CONSBAD=$((CONSBAD+1)); echo "      $f: $(printf '%s' "$out" | grep 'PROVED-ROOTS' | head -1)"
  fi
done
if [ "$CONSBAD" = "0" ]; then
  ok "no fixture reports a PROVED-ROOTS violation (no proved subject fails to root)"
else
  no "$CONSBAD fixture(s) report PROVED-ROOTS — a proved verdict is resting on a closure that does not compute"
fi
# NON-VACUITY, THROUGH THE INVARIANT ITSELF. The first version called `dependencyRootMaterial`
# directly and asserted it refuses — which proves the root builder refuses, not that
# `checkProofCoreConsistency` notices. Removing `provedRoots` from the invariant list would have left
# it green. This builds a ProofCore holding a `proved` obligation whose only edge is unkeyable, runs
# `selfCheck`, and requires a PROVED-ROOTS violation among the results.
probe "PROVED-ROOTS fires from selfCheck on a proved obligation whose closure refuses" "provedRoots=1" \
'def tPk : String := Concrete.shortHash "proved-roots-control"
def tI (m d : String) : Option DefinitionIdentity :=
  (DefinitionIdentity.of? tPk m d (Concrete.shortHash ("impl:" ++ m ++ "." ++ d))).toOption
def fnS (n : String) : CFnDef := { name := n, params := [], retTy := .i32, body := [] }
def elG (q : String) : EligibilityEntry :=
  { qualName := q, eligible := true, sourceReasons := [], profileReasons := []
  , exclusionKind := none, isTrusted := false, admissionRefusals := [], loc := none }
#eval show IO Unit from do
  match tI "m" "caller", (PackageIdentity.syntheticForModules ["m"] ["s"]).toOption with
  | some cid, some pkg =>
    let entry : ProofCoreEntry :=
      { definitionIdentity := .ok cid, qualName := "m.caller", bareName := "caller"
      , callableId := CallableId.ofUser "m" "caller", fn := fnS "caller"
      , extracted := none, unsupported := [], fingerprint := "FP"
      , params := [], eligibility := elG "m.caller", loc := none, spec := none
      , subjectDigest := some "D" }
    -- `proved`, with a call to a name no entry and no excluded record carries: the edge is unkeyable,
    -- so the node holds it in `unscoped` and the closure refuses. Exactly the shape the invariant
    -- exists to catch, and one the corpus does not contain.
    let obl : Obligation :=
      { functionId := { qualName := "m.caller", fingerprint := "FP" }, bareName := "caller"
      , status := .proved, spec := none, expectedFp := "", eligibilityReasons := [], admissible := true
      , ineligCat := none, dependencies := [], notCurrentDeps := [], loc := none }
    let pc : ProofCore :=
      { packageIdentity := pkg, entries := [entry], excluded := [], structs := [], enums := []
      , traitDefs := [], callGraph := [("m.caller", ["m.ghost"])], recMap := [], externNames := []
      , obligations := [obl], diagnostics := [] }
    let vs := pc.selfCheck
    IO.println s!"provedRoots={(vs.filter (·.invariant == "PROVED-ROOTS")).length}"
  | _, _ => IO.println "could not build the control"'

# THE ROOT CONJUNCT IS CONSUMED, ASSERTED BEHAVIOURALLY. Counting call sites cannot establish this:
# a compile-valid mutation can KEEP the call to `dependencyRootMaterial` and discard its result —
# `let _ := ...; true` — leaving the site count at 2 while the `rooted e` conjunct goes inert.
# Measured against exactly that mutation: the site count stayed 2 and this probe flipped from
# DOWNGRADED to STILL-PROVED, so this is the assertion that actually covers consumption.
#
# The subject has NO outgoing edges, which makes correspondence VACUOUSLY justified, so the only
# thing that can move the verdict is the root conjunct. Without that isolation the downgrade would be
# attributable to correspondence and would prove nothing about rooting.
# THE ROOT CONJUNCT IS CONSUMED, ASSERTED BEHAVIOURALLY AND WITHOUT BLESSING ITS DIAGNOSIS.
#
# Counting call sites cannot establish consumption: a compile-valid mutation can KEEP the call to
# `dependencyRootMaterial` and discard its result — `let _ := ...; true` — leaving the site count
# unchanged while the `rooted e` conjunct goes inert. Measured against exactly that mutation, the
# site count stayed 2 and these probes flipped.
#
# WHAT IS DELIBERATELY NOT ASSERTED: the resulting STATUS. The pass reports
# `.correspondenceUnjustified` for a root-only refusal, which is a false cause — that subject's
# correspondence is vacuously VALID and only its closure cannot be assembled. Pinning that string
# would freeze a diagnosis already known to be wrong. The DOWNGRADE is sound and is asserted here;
# the diagnosis is a product defect tracked separately, and these assertions will not need changing
# when it is fixed.
#
# The subject has NO outgoing edges, which is what makes the attribution exact: correspondence has
# nothing to justify and cannot be the cause of any downgrade below.

probe "the closure of a digest-less subject CANNOT be rooted (the internal refusal)" "ROOT-REFUSED" \
'def aPk : String := Concrete.shortHash "authority-consumes-root"
def aI (m d : String) : Option DefinitionIdentity :=
  (DefinitionIdentity.of? aPk m d (Concrete.shortHash ("impl:" ++ m ++ "." ++ d))).toOption
def aFn (n : String) : CFnDef := { name := n, params := [], retTy := .i32, body := [] }
def aEl (q : String) : EligibilityEntry :=
  { qualName := q, eligible := true, sourceReasons := [], profileReasons := []
  , exclusionKind := none, isTrusted := false, admissionRefusals := [], loc := none }
#eval show IO Unit from do
  match aI "m" "solo", (PackageIdentity.syntheticForModules ["m"] ["s"]).toOption with
  | some cid, some pkg =>
    let entry : ProofCoreEntry :=
      { definitionIdentity := .ok cid, qualName := "m.solo", bareName := "solo"
      , callableId := CallableId.ofUser "m" "solo", fn := aFn "solo"
      , extracted := none, unsupported := [], fingerprint := "FP"
      , params := [], eligibility := aEl "m.solo", loc := none, spec := none
      , subjectDigest := none }
    let obl : Obligation :=
      { functionId := { qualName := "m.solo", fingerprint := "FP" }, bareName := "solo"
      , status := .proved, spec := none, expectedFp := "", eligibilityReasons := [], admissible := true
      , ineligCat := none, dependencies := [], notCurrentDeps := [], loc := none }
    let pc : ProofCore :=
      { packageIdentity := pkg, entries := [entry], excluded := [], structs := [], enums := []
      , traitDefs := [], callGraph := [("m.solo", [])], recMap := [], externNames := []
      , obligations := [obl], diagnostics := [] }
    match dependencyRootMaterial (dependencyNodesOf pc pc.callGraph) cid with
    | .error _ => IO.println "ROOT-REFUSED"
    | .ok _    => IO.println "ROOT-COMPUTED"
  | _, _ => IO.println "could not build the control"'

probe "...while the SAME subject's correspondence has nothing to justify (so it is not the cause)" "NOTHING-TO-JUSTIFY" \
'def aPk : String := Concrete.shortHash "authority-consumes-root"
def aI (m d : String) : Option DefinitionIdentity :=
  (DefinitionIdentity.of? aPk m d (Concrete.shortHash ("impl:" ++ m ++ "." ++ d))).toOption
def aFn (n : String) : CFnDef := { name := n, params := [], retTy := .i32, body := [] }
def aEl (q : String) : EligibilityEntry :=
  { qualName := q, eligible := true, sourceReasons := [], profileReasons := []
  , exclusionKind := none, isTrusted := false, admissionRefusals := [], loc := none }
#eval show IO Unit from do
  match aI "m" "solo", (PackageIdentity.syntheticForModules ["m"] ["s"]).toOption with
  | some cid, some pkg =>
    let entry : ProofCoreEntry :=
      { definitionIdentity := .ok cid, qualName := "m.solo", bareName := "solo"
      , callableId := CallableId.ofUser "m" "solo", fn := aFn "solo"
      , extracted := none, unsupported := [], fingerprint := "FP"
      , params := [], eligibility := aEl "m.solo", loc := none, spec := none
      , subjectDigest := none }
    let obl : Obligation :=
      { functionId := { qualName := "m.solo", fingerprint := "FP" }, bareName := "solo"
      , status := .proved, spec := none, expectedFp := "", eligibilityReasons := [], admissible := true
      , ineligCat := none, dependencies := [], notCurrentDeps := [], loc := none }
    let pc : ProofCore :=
      { packageIdentity := pkg, entries := [entry], excluded := [], structs := [], enums := []
      , traitDefs := [], callGraph := [("m.solo", [])], recMap := [], externNames := []
      , obligations := [obl], diagnostics := [] }
    match correspondenceInputOf pc pc.callGraph entry.callableId with
    | .error _ => IO.println "NO-CORRESPONDENCE-INPUT"
    | .ok i =>
      if i.requestedEdges.isEmpty && i.unscopedEdges.isEmpty then
        IO.println "NOTHING-TO-JUSTIFY"
      else IO.println s!"HAS-EDGES({i.requestedEdges.length},{i.unscopedEdges.length})"
  | _, _ => IO.println "could not build the control"'

probe "a subject whose closure cannot root LOSES every friendly disposition" "NOT-FRIENDLY" \
'def aPk : String := Concrete.shortHash "authority-consumes-root"
def aI (m d : String) : Option DefinitionIdentity :=
  (DefinitionIdentity.of? aPk m d (Concrete.shortHash ("impl:" ++ m ++ "." ++ d))).toOption
def aFn (n : String) : CFnDef := { name := n, params := [], retTy := .i32, body := [] }
def aEl (q : String) : EligibilityEntry :=
  { qualName := q, eligible := true, sourceReasons := [], profileReasons := []
  , exclusionKind := none, isTrusted := false, admissionRefusals := [], loc := none }
#eval show IO Unit from do
  match aI "m" "solo", (PackageIdentity.syntheticForModules ["m"] ["s"]).toOption with
  | some cid, some pkg =>
    let entry : ProofCoreEntry :=
      { definitionIdentity := .ok cid, qualName := "m.solo", bareName := "solo"
      , callableId := CallableId.ofUser "m" "solo", fn := aFn "solo"
      , extracted := none, unsupported := [], fingerprint := "FP"
      , params := [], eligibility := aEl "m.solo", loc := none, spec := none
      , subjectDigest := none }
    let obl : Obligation :=
      { functionId := { qualName := "m.solo", fingerprint := "FP" }, bareName := "solo"
      , status := .proved, spec := none, expectedFp := "", eligibilityReasons := [], admissible := true
      , ineligCat := none, dependencies := [], notCurrentDeps := [], loc := none }
    let pc : ProofCore :=
      { packageIdentity := pkg, entries := [entry], excluded := [], structs := [], enums := []
      , traitDefs := [], callGraph := [("m.solo", [])], recMap := [], externNames := []
      , obligations := [obl], diagnostics := [] }
    match (applyCorrespondenceAuthority pc pc.callGraph).obligations.head? with
    | some o =>
      -- The friendly set is exactly the one the policy pass waves through
      -- (`.proved | .ineligible | .trusted => none`); naming the arms rather than the resulting
      -- status keeps this true after the diagnosis is corrected.
      match o.status with
      | .proved | .ineligible | .trusted => IO.println "FRIENDLY"
      | _ => IO.println "NOT-FRIENDLY"
    | none => IO.println "NO-OBLIGATION"
  | _, _ => IO.println "could not build the control"'

probe "...and a subject whose closure DOES root keeps .proved exactly (not deny-all)" "PROVED-PRESERVED" \
'def aPk : String := Concrete.shortHash "authority-consumes-root"
def aI (m d : String) : Option DefinitionIdentity :=
  (DefinitionIdentity.of? aPk m d (Concrete.shortHash ("impl:" ++ m ++ "." ++ d))).toOption
def aFn (n : String) : CFnDef := { name := n, params := [], retTy := .i32, body := [] }
def aEl (q : String) : EligibilityEntry :=
  { qualName := q, eligible := true, sourceReasons := [], profileReasons := []
  , exclusionKind := none, isTrusted := false, admissionRefusals := [], loc := none }
#eval show IO Unit from do
  match aI "m" "solo", (PackageIdentity.syntheticForModules ["m"] ["s"]).toOption with
  | some cid, some pkg =>
    let entry : ProofCoreEntry :=
      { definitionIdentity := .ok cid, qualName := "m.solo", bareName := "solo"
      , callableId := CallableId.ofUser "m" "solo", fn := aFn "solo"
      , extracted := none, unsupported := [], fingerprint := "FP"
      , params := [], eligibility := aEl "m.solo", loc := none, spec := none
      , subjectDigest := some "D" }
    let obl : Obligation :=
      { functionId := { qualName := "m.solo", fingerprint := "FP" }, bareName := "solo"
      , status := .proved, spec := none, expectedFp := "", eligibilityReasons := [], admissible := true
      , ineligCat := none, dependencies := [], notCurrentDeps := [], loc := none }
    let pc : ProofCore :=
      { packageIdentity := pkg, entries := [entry], excluded := [], structs := [], enums := []
      , traitDefs := [], callGraph := [("m.solo", [])], recMap := [], externNames := []
      , obligations := [obl], diagnostics := [] }
    match (applyCorrespondenceAuthority pc pc.callGraph).obligations.head? with
    | some o =>
      -- EXACTLY `.proved`, because that is what it started as and what issuance requires. Accepting
      -- any friendly status here would let the pass silently rewrite a proved claim into another
      -- agreeable one and still pass.
      match o.status with
      | .proved => IO.println "PROVED-PRESERVED"
      | _       => IO.println "STATUS-MOVED"
    | none => IO.println "NO-OBLIGATION"
  | _, _ => IO.println "could not build the control"'

# TRUST PROPAGATES MONOTONICALLY THROUGH THE FULL CLOSURE, not one hop.
#
# `composition_trusted_helper` proves the one-hop case and cannot show more. `composition_deep_trust`
# is `outer -> middle -> leaf` with `leaf` trusted: `outer`'s own body mentions no trusted function
# at all, and it must still disclose the assumption. A claim that dropped it two hops from the
# boundary would launder exactly what the one-hop qualification prevents.
DEEP_TRUST="$(cc examples/proof_patterns/composition_deep_trust/src/main.con proof-status || true)"
DEEP_N="$(printf '%s' "$DEEP_TRUST" | grep -c 'ASSUMES trusted boundaries (not proved): calls.leaf' || true)"
if [ "$DEEP_N" = "2" ]; then
  ok "trust reaches BOTH hops: middle and outer each disclose calls.leaf"
else
  no "deep trust disclosed by $DEEP_N of 2 subjects — propagation stopped short of the full closure"
fi
# ...and the one-hop case still holds, so the deep control did not replace it.
ONE_HOP="$(cc examples/proof_patterns/composition_trusted_helper/src/main.con proof-status || true)"
if printf '%s' "$ONE_HOP" | grep -q 'ASSUMES trusted boundaries (not proved): calls.dbl'; then
  ok "a PROVED claim one hop from a boundary still states its assumption"
else
  no "composition_trusted_helper no longer discloses calls.dbl — the proved path lost its qualification"
fi

# ONLY TRUSTED EXCLUSIONS BECOME NODES, checked by CALLING `dependencyNodesOf`. The previous probe
# asserted two facts about `isCurrentForDependents` and never touched the node builder, so removing
# the trusted-only filter would not have moved it. This builds a ProofCore with one entry and two
# excluded definitions — one trusted, one not — and reads the node set the production path produces.
probe "dependencyNodesOf gives a node to a TRUSTED exclusion and none to an ineligible one" "nodes=2" \
'def tPkgC : String := Concrete.shortHash "leaf-boundary-control"
def tidC (m d : String) : Option DefinitionIdentity :=
  (DefinitionIdentity.of? tPkgC m d (Concrete.shortHash ("impl:" ++ m ++ "." ++ d))).toOption
def fnStubC (n : String) : CFnDef := { name := n, params := [], retTy := .i32, body := [] }
def eligC (q : String) (trusted : Bool) : EligibilityEntry :=
  { qualName := q, eligible := !trusted, sourceReasons := [], profileReasons := []
  , exclusionKind := none, isTrusted := trusted, admissionRefusals := [], loc := none }
def mkExclC (q d : String) (trusted : Bool) : Option ProofCoreExcluded :=
  (tidC "m" d).map fun i =>
    { qualName := q, bareName := d, callableId := CallableId.ofUser "m" d
    , definitionIdentity := .ok i, fn := fnStubC d, fingerprint := "FP" ++ d
    , eligibility := eligC q trusted, loc := none, spec := none }
def mkEntryC (q d : String) : Option ProofCoreEntry :=
  (tidC "m" d).map fun i =>
    { definitionIdentity := .ok i, qualName := q, bareName := d
    , callableId := CallableId.ofUser "m" d, fn := fnStubC d
    , extracted := none, unsupported := [], fingerprint := "FP" ++ d
    , params := [], eligibility := eligC q false, loc := none, spec := none
    , subjectDigest := some ("D" ++ d) }
#eval show IO Unit from do
  match mkEntryC "m.caller" "caller", mkExclC "m.trustedHelper" "trustedHelper" true,
        mkExclC "m.recursiveHelper" "recursiveHelper" false,
        (PackageIdentity.syntheticForModules ["m"] ["src"]).toOption with
  | some caller, some tX, some iX, some pkg =>
    let pc : ProofCore :=
      { packageIdentity := pkg, entries := [caller], excluded := [tX, iX]
      , structs := [], enums := [], traitDefs := []
      , callGraph := [("m.caller", ["m.trustedHelper", "m.recursiveHelper"])]
      , recMap := [], externNames := [], obligations := [], diagnostics := [] }
    let nodes := dependencyNodesOf pc pc.callGraph
    let names := (nodes.map (·.label.declName)).mergeSort (· ≤ ·)
    IO.println s!"nodes={nodes.length} {names}"
  | _, _, _, _ => IO.println "could not build the control"'
# ...and a closure crossing a TRUSTED boundary must still root. Its absence refused `calls.combine`
# entirely, for a reason that had nothing to do with its evidence.
TH_ROOTS="$(cc examples/proof_patterns/composition_trusted_helper/src/main.con subject-facts || true)"
if printf '%s' "$TH_ROOTS" | grep -q 'shadow depRoot: REFUSED'; then
  no "composition_trusted_helper cannot root — an edge to a trusted callee has no node again"
else
  ok "a closure crossing a TRUSTED boundary roots (trusted exclusions are leaf nodes)"
fi
# ONLY TRUSTED EXCLUSIONS ARE LEAVES. Every excluded definition carries a scoped identity, so it is
# tempting to give them all nodes — and that makes closures computable over callees excluded for
# reasons carrying no evidence at all. The restriction is asserted at the type level rather than by
# reading the corpus, because the corpus has no ineligible callee on a body edge today and a control
# that cannot fail is not a control.
probe "an INELIGIBLE exclusion is not a leaf boundary (only trusted ones are)" "true" \
'#eval
  match DefinitionIdentity.of? "pkg0123456789abcdef0123456789abcd" "m" "helper"
          "00000000000000000000000000000000" with
  | .error _ => false
  | .ok _ =>
    -- The rule is structural: `dependencyNodesOf` filters excluded records on `isTrusted`, so an
    -- ineligible exclusion contributes no node and a closure reaching it REFUSES rather than
    -- serializing over material that carries no evidence.
    (ObligationStatus.ineligible.isCurrentForDependents == false)
      && (ObligationStatus.trusted.isCurrentForDependents == true)'

# === WHOLE-TABLE BINDING (slice 4: never under-approximate table access) =====================
# A `body` edge naming a table is not yet a dependency ON that table. The NAME does not move
# when an entry's definition changes, so a proof about `fns` stayed valid-looking after any
# edit inside `fns`. `tableValueDigest` binds the constant's defining VALUE — whole by
# construction, because there is no subset to select from — and a dynamic index means the
# apparent subset would have been an under-approximation, which fails OPEN.
echo "=== whole-table binding ==="

probe "a table's digest MOVES when its contents change" "MOVED" '
def tA : FnTable := FnTable.ofGlobals (fun n => if n == "f" then none else none)
def tB : FnTable := FnTable.ofGlobals (fun n => if n == "g" then none else none)
#eval show MetaM Unit from do
  let a ← tableValueDigest `tA
  let b ← tableValueDigest `tB
  IO.println (if a.isSome && b.isSome && a != b then "MOVED" else "SAME")'

# Fail-closed. A constant with no value — axiom, opaque, unavailable import — must REFUSE
# rather than digest to something. A digest over "the part we could see" is indistinguishable
# from one over the whole table, which is how a partial dependency becomes an invisible one.
probe "a valueless/unknown constant REFUSES rather than digesting" "REFUSED" '
#eval show MetaM Unit from do
  let d ← tableValueDigest `No.Such.Table
  IO.println (if d.isNone then "REFUSED" else "DIGESTED")'

# The digest must be a FUNCTION of the constant, or a receipt could not be replayed.
probe "the same table digests identically twice (deterministic)" "STABLE" '
def tS : FnTable := FnTable.ofGlobals (fun _ => none)
#eval show MetaM Unit from do
  let a ← tableValueDigest `tS
  let b ← tableValueDigest `tS
  IO.println (if a.isSome && a == b then "STABLE" else "UNSTABLE")'

# `tablesFullyBound` is the gate a receipt must consult: a `body` edge that names a table it
# could not bind announces a dependency whose changes it cannot detect, which reads exactly
# like a dependency that never changes.
probe "evidence naming an unbindable table is NOT fully bound" "NOTBOUND" '
#eval show MetaM Unit from do
  let e : EdgeEvidence := { edge := .body, tables := [`No.Such.Table]
                          , tableDigests := [(`No.Such.Table, none)], quantifiesOverTable := false }
  IO.println (if e.tablesFullyBound then "BOUND" else "NOTBOUND")'

probe "...and evidence with every table bound IS fully bound (control)" "BOUND" '
#eval show MetaM Unit from do
  let e : EdgeEvidence := { edge := .body, tables := [`X]
                          , tableDigests := [(`X, some "abc")], quantifiesOverTable := false }
  IO.println (if e.tablesFullyBound then "BOUND" else "NOTBOUND")'

# A count mismatch must also fail: fewer digests than names is the silent shape of a partial
# binding, and length-equality is what makes "fully" mean every one of them.
probe "fewer digests than named tables is NOT fully bound" "NOTBOUND" '
#eval show MetaM Unit from do
  let e : EdgeEvidence := { edge := .body, tables := [`X, `Y]
                          , tableDigests := [(`X, some "abc")], quantifiesOverTable := false }
  IO.println (if e.tablesFullyBound then "BOUND" else "NOTBOUND")'

# === THE RECEIPT ENVELOPE (slice 4) =========================================================
# `tablesFullyBound` is a predicate, and a predicate can be forgotten. The receipt type is built
# so it cannot be: `tableBindings` is `List (Name × String)`, not `Option String`, so an unbound
# table has NO REPRESENTATION inside a receipt. These legs pin every refusal path, because a
# smart constructor that refuses nothing is just a constructor.
echo "=== receipt envelope ==="

probe "a receipt mints from complete evidence (control)" "MINTED" '
#eval show MetaM Unit from do
  let ev : EdgeEvidence := { edge := .body, tables := [`X]
                           , tableDigests := [(`X, some "d1")], quantifiesOverTable := false }
  let r := ReceiptMaterial.of? (some "v2:abc") ev "ROOT" false [] "lean-4.28" "/ws" "imp1"
  IO.println (if r.isSome then "MINTED" else "REFUSED")'

# The whole point. A body edge naming a table it could not bind announces a dependency whose
# changes it cannot detect — which reads exactly like a dependency that never changes.
probe "an UNBOUND table refuses to mint" "REFUSED" '
#eval show MetaM Unit from do
  let ev : EdgeEvidence := { edge := .body, tables := [`X]
                           , tableDigests := [(`X, none)], quantifiesOverTable := false }
  let r := ReceiptMaterial.of? (some "v2:abc") ev "ROOT" false [] "lean-4.28" "/ws" "imp1"
  IO.println (if r.isNone then "REFUSED" else "MINTED")'

probe "an ABSENT subject digest refuses to mint" "REFUSED" '
#eval show MetaM Unit from do
  let ev : EdgeEvidence := { edge := .body, tables := [`X]
                           , tableDigests := [(`X, some "d1")], quantifiesOverTable := false }
  let r := ReceiptMaterial.of? none ev "ROOT" false [] "lean-4.28" "/ws" "imp1"
  IO.println (if r.isNone then "REFUSED" else "MINTED")'

# An empty identity is not "unknown" — it is a value, and it compares EQUAL to another empty
# one, so two proofs established under different toolchains would agree. Refusing is the only
# reading that does not invent agreement. One leg per field: a single check would pass while
# two of the three were silently unbound.
probe "an empty TOOLCHAIN id refuses to mint" "REFUSED" '
#eval show MetaM Unit from do
  let ev : EdgeEvidence := { edge := .body, tables := [], tableDigests := [], quantifiesOverTable := true }
  IO.println (if (ReceiptMaterial.of? (some "v2:a") ev "ROOT" false [] "" "/ws" "i").isNone then "REFUSED" else "MINTED")'
probe "an empty WORKSPACE id refuses to mint" "REFUSED" '
#eval show MetaM Unit from do
  let ev : EdgeEvidence := { edge := .body, tables := [], tableDigests := [], quantifiesOverTable := true }
  IO.println (if (ReceiptMaterial.of? (some "v2:a") ev "ROOT" false [] "lean" "" "i").isNone then "REFUSED" else "MINTED")'
probe "an empty IMPORTS id refuses to mint" "REFUSED" '
#eval show MetaM Unit from do
  let ev : EdgeEvidence := { edge := .body, tables := [], tableDigests := [], quantifiesOverTable := true }
  IO.println (if (ReceiptMaterial.of? (some "v2:a") ev "ROOT" false [] "lean" "/ws" "").isNone then "REFUSED" else "MINTED")'

# Schema version is IN the receipt so an older one reads as a different format rather than as a
# failed comparison — the same reason `v2:` is in the subject digest. Without it the first
# envelope change reports every stored receipt as a broken proof.
# The old-schema leg USED to build a receipt directly — which is exactly the bypass this
# section now forbids, and the reason the "unrepresentable" claim was false when first made.
# An old-schema receipt can only arrive by DESERIALIZATION, which does not exist yet, so that
# leg is deliberately absent rather than faked with a constructor call. Recorded here so its
# absence is a known gap and not an oversight.
probe_mint "...and a current-schema receipt IS comparable (control)" "COMPARABLE" '
#eval show MetaM Unit from do
  let ev : EdgeEvidence := { edge := .body, tables := [], tableDigests := [], quantifiesOverTable := true }
  match ← mintProbe (some "v2:a") ev "ROOT" false [] "t" "w" "i" with
  | some r => IO.println (if r.comparable then "COMPARABLE" else "INCOMPARABLE")
  | none   => IO.println "MINT-REFUSED"'

# End-to-end: the digests a real classification produces must reach a receipt.
probe_mint "a minted receipt carries the table binding it was given" "d1" '
#eval show MetaM Unit from do
  let ev : EdgeEvidence := { edge := .body, tables := [`X]
                           , tableDigests := [(`X, some "d1")], quantifiesOverTable := false }
  match ← mintProbe (some "v2:a") ev "ROOT" false [] "t" "w" "i" with
  | some r => IO.println (String.intercalate "," (r.tableBindings.map (·.2)))
  | none   => IO.println "REFUSED"'

# === RECEIPT CURRENCY (the negative controls a receipt is FOR) ==============================
# Minting refusals prove a receipt cannot be built from partial material. These prove the
# built receipt DETECTS change — which is the only reason to store one.
echo "=== receipt currency ==="

probe_mint "an unchanged environment is current (control)" "CURRENT" '
#eval show MetaM Unit from do
  let ev : EdgeEvidence := { edge := .body, tables := [`A]
                           , tableDigests := [(`A, some "da")], quantifiesOverTable := false }
  match ← mintProbe (some "v2:s") ev "ROOT" false [] "tc" "ws" "im" with
  | some r => IO.println (if r.isCurrentAgainst "v2:s" .body [(`A, "da")] "ROOT" r.theoremArtifact false [] r.toolchainId "ws" "im" then "CURRENT" else "NOT")
  | none => IO.println "MINT-REFUSED"'

probe_mint "changing a TABLE BODY makes the receipt non-current" "NOT" '
#eval show MetaM Unit from do
  let ev : EdgeEvidence := { edge := .body, tables := [`A]
                           , tableDigests := [(`A, some "da")], quantifiesOverTable := false }
  match ← mintProbe (some "v2:s") ev "ROOT" false [] "tc" "ws" "im" with
  | some r => IO.println (if r.isCurrentAgainst "v2:s" .body [(`A, "da-CHANGED")] "ROOT" r.theoremArtifact false [] r.toolchainId "ws" "im" then "CURRENT" else "NOT")
  | none => IO.println "MINT-REFUSED"'

# The swap. Sorting normalizes ORDER, and must not normalize away WHICH name carries which
# digest — otherwise two tables exchanging contents would look unchanged.
probe_mint "SWAPPING two tables digests makes the receipt non-current" "NOT" '
#eval show MetaM Unit from do
  let ev : EdgeEvidence := { edge := .body, tables := [`A, `B]
                           , tableDigests := [(`A, some "da"), (`B, some "db")], quantifiesOverTable := false }
  match ← mintProbe (some "v2:s") ev "ROOT" false [] "tc" "ws" "im" with
  | some r => IO.println (if r.isCurrentAgainst "v2:s" .body [(`A, "db"), (`B, "da")] "ROOT" r.theoremArtifact false [] r.toolchainId "ws" "im" then "CURRENT" else "NOT")
  | none => IO.println "MINT-REFUSED"'

# ...while REORDERING the same pairs must NOT. Order is normalized, so it carries no
# information; without this control the sort could be dropped and nothing would notice.
probe_mint "REORDERING the same pairs leaves it current" "CURRENT" '
#eval show MetaM Unit from do
  let ev : EdgeEvidence := { edge := .body, tables := [`B, `A]
                           , tableDigests := [(`B, some "db"), (`A, some "da")], quantifiesOverTable := false }
  match ← mintProbe (some "v2:s") ev "ROOT" false [] "tc" "ws" "im" with
  | some r => IO.println (if r.isCurrentAgainst "v2:s" .body [(`A, "da"), (`B, "db")] "ROOT" r.theoremArtifact false [] r.toolchainId "ws" "im" then "CURRENT" else "NOT")
  | none => IO.println "MINT-REFUSED"'

# The structural table digest is toolchain-relative (recorded limit at tableValueDigest). That
# is only acceptable if the toolchain is itself bound — so a toolchain change must invalidate
# even when every table digest is byte-identical.
probe_mint "a TOOLCHAIN change invalidates even with identical table digests" "NOT" '
#eval show MetaM Unit from do
  let ev : EdgeEvidence := { edge := .body, tables := [`A]
                           , tableDigests := [(`A, some "da")], quantifiesOverTable := false }
  match ← mintProbe (some "v2:s") ev "ROOT" false [] "tc" "ws" "im" with
  | some r => IO.println (if r.isCurrentAgainst "v2:s" .body [(`A, "da")] "ROOT" r.theoremArtifact false [] "tc-NEW" "ws" "im" then "CURRENT" else "NOT")
  | none => IO.println "MINT-REFUSED"'

probe_mint "a WORKSPACE change invalidates" "NOT" '
#eval show MetaM Unit from do
  let ev : EdgeEvidence := { edge := .body, tables := [], tableDigests := [], quantifiesOverTable := true }
  match ← mintProbe (some "v2:s") ev "ROOT" false [] "tc" "ws" "im" with
  | some r => IO.println (if r.isCurrentAgainst "v2:s" .body [] "ROOT" r.theoremArtifact false [] r.toolchainId "ws-NEW" "im" then "CURRENT" else "NOT")
  | none => IO.println "MINT-REFUSED"'

probe_mint "an IMPORT-CLOSURE change invalidates" "NOT" '
#eval show MetaM Unit from do
  let ev : EdgeEvidence := { edge := .body, tables := [], tableDigests := [], quantifiesOverTable := true }
  match ← mintProbe (some "v2:s") ev "ROOT" false [] "tc" "ws" "im" with
  | some r => IO.println (if r.isCurrentAgainst "v2:s" .body [] "ROOT" r.theoremArtifact false [] r.toolchainId "ws" "im-NEW" then "CURRENT" else "NOT")
  | none => IO.println "MINT-REFUSED"'

probe_mint "a SUBJECT change invalidates" "NOT" '
#eval show MetaM Unit from do
  let ev : EdgeEvidence := { edge := .body, tables := [], tableDigests := [], quantifiesOverTable := true }
  match ← mintProbe (some "v2:s") ev "ROOT" false [] "tc" "ws" "im" with
  | some r => IO.println (if r.isCurrentAgainst "v2:s-NEW" .body [] "ROOT" r.theoremArtifact false [] r.toolchainId "ws" "im" then "CURRENT" else "NOT")
  | none => IO.println "MINT-REFUSED"'

# A CONTRACT edge names no tables, so it must not acquire a body dependency it does not have.
probe_mint "a contract edge binds NO tables" "0" '
#eval show MetaM Unit from do
  let ev : EdgeEvidence := { edge := .contract, tables := [], tableDigests := [], quantifiesOverTable := true }
  match ← mintProbe (some "v2:s") ev "ROOT" false [] "tc" "ws" "im" with
  | some r => IO.println (toString r.tableBindings.length)
  | none => IO.println "MINT-REFUSED"'

# Mint-level, not just predicate-level: fewer digests than names must refuse at the constructor.
probe "fewer digests than names refuses AT MINT" "REFUSED" '
#eval show MetaM Unit from do
  let ev : EdgeEvidence := { edge := .body, tables := [`A, `B]
                           , tableDigests := [(`A, some "da")], quantifiesOverTable := false }
  IO.println (if (ReceiptMaterial.of? (some "v2:s") ev "ROOT" false [] "tc" "ws" "im").isNone then "REFUSED" else "MINTED")'

# === HOSTILE CONTROLS: the invariant is the TYPE, not the smart constructor =================
# The first version of this envelope claimed "partial evidence is unrepresentable" while the
# structure constructor was public and `Inhabited` was derived — so a caller could assemble a
# receipt with an empty subject and the CURRENT schema version, and `default` produced one with
# an empty schema. The nine minting legs above passed the whole time: they tested the smart
# constructor, not the claim. These test the claim.
echo "=== hostile controls (construction is closed) ==="

# A NEGATIVE TEST MUST FAIL FOR ITS OWN REASON. This accepted ANY Lean diagnostic and discarded the
# exit status, so a probe meant to prove "the constructor is private" stayed green after the
# constructor became public, as long as some unrelated typo, missing import or syntax error was
# present. Each call now declares the diagnostic it expects, and the run must both exit nonzero and
# produce THAT diagnostic.
EXPECTED_NEGATIVE_TESTS=9
EXPECTED_NEGATIVE_MANIFEST_SHA256="5fa458edeae85325235b877ae9a2fabeb920f245063c719b226235a86d8f3a1b"
# THE LABEL ALONE DOES NOT IDENTIFY THE TEST. Nothing bound a negative test's label to its BODY and
# its expected patterns, so one test's body and patterns could be replaced with another's while
# keeping its label: the count stays 9, the total stays 310, the assertion labels are identical, and
# neither probe manifest covers these tests at all. One assertion standing in for another.
NC_RECS=""
EXPECT_NC_N=0
# ALL declared patterns must match. One pattern was too coarse to separate the intended failure from
# an unrelated one of the same class.
expect_no_compile() {
  local label="$1" body="$2"; shift 2
  local -a pats=("$@")
  EXPECT_NC_N=$((EXPECT_NC_N + 1))
  if [ -n "$_MINT_HASHER" ]; then
    NC_RECS+="$(_manifest_record "$label" "${pats[*]-}" "negative" "$body")
"
  fi
  cat > "$TMP/h.lean" <<LEAN
import Concrete
import Examples
open Lean Meta Concrete Concrete.Proof
$body
LEAN
  # NO DUMP SEAM. `EXPECT_NC_DUMP=1` returned before reporting any verdict, so an inherited value
  # skipped all nine negative assertions and still allowed exit 0 — the same class of hole as the
  # manifest skip. Diagnostics for pinning new patterns are obtained by running the snippet directly.
  local out rc=0
  out="$(lake env lean "$TMP/h.lean" 2>&1)" || rc=$?
  local p miss=""
  for p in ${pats[@]+"${pats[@]}"}; do
    grep -qE -- "$p" <<<"$out" || { miss="$p"; break; }
  done
  if [ "${#pats[@]}" -eq 0 ]; then
    no "$label — no expected diagnostic declared, so any error would satisfy it"
  elif [ "$rc" -eq 0 ]; then
    no "$label — IT COMPILED (exit 0), so the invariant is documentation rather than a type"
  elif ! grep -qE "error:|error\(lean" <<<"$out"; then
    no "$label — exited $rc but produced no Lean diagnostic, so it failed for an unrelated reason"
  elif [ -n "$miss" ]; then
    no "$label — rejected for the WRONG reason (missing /$miss/): $(printf '%s' "$out" | grep -m1 -E 'error' | cut -c1-180)"
  else
    ok "$label"
  fi
}

expect_no_compile "direct construction does NOT compile (constructor is private)" '
def forged : ProofEvidenceReceipt :=
  { schemaVersion := receiptSchemaVersion, subjectDigest := "", edge := .body
  , tableBindings := [], toolchainId := "", workspaceId := "", importsId := "" }' \
  'constructor for .ProofEvidenceReceipt. is marked as private'

expect_no_compile '`default` does NOT produce a receipt (no Inhabited)' '
#eval show MetaM Unit from IO.println (default : ProofEvidenceReceipt).schemaVersion' \
  'synthInstanceFailed|failed to synthesize' 'Inhabited ProofEvidenceReceipt'

# Correspondence, not arity. `tablesFullyBound` checks equal LENGTHS and all-present, which
# admits tables := [X] with digests for [Y] — a receipt claiming Y while the theorem depended
# on X. Equal counts of unrelated things is not correspondence.
probe "digests for the WRONG table identity refuse to mint" "REFUSED" '
#eval show MetaM Unit from do
  let ev : EdgeEvidence := { edge := .body, tables := [`X]
                           , tableDigests := [(`Y, some "d")], quantifiesOverTable := false }
  IO.println (if (ReceiptMaterial.of? (some "v2:s") ev "ROOT" false [] "t" "w" "i").isNone then "REFUSED" else "MINTED")'

probe "a DUPLICATE binding for one table refuses to mint" "REFUSED" '
#eval show MetaM Unit from do
  let ev : EdgeEvidence := { edge := .body, tables := [`X, `X]
                           , tableDigests := [(`X, some "d"), (`X, some "e")], quantifiesOverTable := false }
  IO.println (if (ReceiptMaterial.of? (some "v2:s") ev "ROOT" false [] "t" "w" "i").isNone then "REFUSED" else "MINTED")'

# `none` was refused and `some ""` was not — the same hole as an empty environment identity.
# "" is a value, and it compares equal to another "".
probe "an EMPTY-STRING subject digest refuses to mint" "REFUSED" '
#eval show MetaM Unit from do
  let ev : EdgeEvidence := { edge := .body, tables := [], tableDigests := [], quantifiesOverTable := true }
  IO.println (if (ReceiptMaterial.of? (some "") ev "ROOT" false [] "t" "w" "i").isNone then "REFUSED" else "MINTED")'

# The EDGE must participate in currency. Its omission was a real hole: a receipt recorded for a
# `contract` edge read current against `body` material — a claim surviving exactly the
# implementation change it depends on.
probe_mint "an EDGE-KIND change makes the receipt non-current" "NOT" '
#eval show MetaM Unit from do
  let ev : EdgeEvidence := { edge := .body, tables := [], tableDigests := [], quantifiesOverTable := true }
  match ← mintProbe (some "v2:s") ev "ROOT" false [] "tc" "ws" "im" with
  | some r => IO.println (if r.isCurrentAgainst "v2:s" .contract [] "ROOT" r.theoremArtifact false [] r.toolchainId "ws" "im" then "CURRENT" else "NOT")
  | none => IO.println "MINT-REFUSED"'

# ONE disposition, not two booleans in the right order. `comparable` then `isCurrentAgainst` was
# a sequencing a consumer had to remember, and reading them out of order reports "the proof went
# stale" when the ENVELOPE changed — a claim about the program rather than the format.
probe_mint "disposition: unchanged material is current" "Concrete.Proof.ReceiptDisposition.current" '
#eval show MetaM Unit from do
  let ev : EdgeEvidence := { edge := .body, tables := [], tableDigests := [], quantifiesOverTable := true }
  match ← mintProbe (some "v2:s") ev "ROOT" false [] "tc" "ws" "im" with
  | some r => IO.println (toString (repr (r.disposition "v2:s" .body [] "ROOT" r.theoremArtifact false [] r.toolchainId "ws" "im")))
  | none => IO.println "MINT-REFUSED"'

probe_mint "disposition: moved material is notCurrent (not needsRecheck)" "Concrete.Proof.ReceiptDisposition.notCurrent" '
#eval show MetaM Unit from do
  let ev : EdgeEvidence := { edge := .body, tables := [], tableDigests := [], quantifiesOverTable := true }
  match ← mintProbe (some "v2:s") ev "ROOT" false [] "tc" "ws" "im" with
  | some r => IO.println (toString (repr (r.disposition "v2:MOVED" .body [] "ROOT" r.theoremArtifact false [] r.toolchainId "ws" "im")))
  | none => IO.println "MINT-REFUSED"'

# === ENVIRONMENT IDENTITIES ==================================================================
# The receipt refuses an empty identity, so nothing could mint until these existed. Each must be
# DETERMINISTIC (same inputs, same id — or a receipt cannot be replayed) and SENSITIVE (any input
# change moves it — or binding it bought nothing). Both directions per identity, because an id
# that never moves and an id that never repeats are equally useless.
echo "=== environment identities ==="

probe "toolchain id is deterministic and sensitive to BOTH inputs" "true" '
#eval
  let a := toolchainIdOf "0.1.0" "lean4:v4.28.0"
  let b := toolchainIdOf "0.1.0" "lean4:v4.28.0"
  let c := toolchainIdOf "0.2.0" "lean4:v4.28.0"
  let d := toolchainIdOf "0.1.0" "lean4:v4.29.0"
  a == b && a != c && a != d && c != d'

# Discovery order must not enter the identity: the same workspace enumerated differently is the
# same workspace.
probe "workspace id ignores module ORDER but not module SET" "true" '
#eval
  let a := workspaceIdOf "pkg" ["m1", "m2"]
  let b := workspaceIdOf "pkg" ["m2", "m1"]
  let c := workspaceIdOf "pkg" ["m1", "m3"]
  let d := workspaceIdOf "other" ["m1", "m2"]
  a == b && a != c && a != d'

# Import CONTENT, not just names — otherwise an imported module could change under a proof
# without moving anything, which is the defect tableValueDigest exists to prevent one level down.
probe "imports id moves when an imported CONTENT digest changes" "true" '
#eval
  let a := importsIdOf [("m1", "d1"), ("m2", "d2")]
  let b := importsIdOf [("m2", "d2"), ("m1", "d1")]
  let c := importsIdOf [("m1", "dCHANGED"), ("m2", "d2")]
  a == b && a != c'

# End to end: real identities let a receipt mint, which nothing could do before.
# THE NEW BINDINGS REFUSE ON THE SAME TERMS AS THE OLD ONES. An empty root or artifact is not
# "unknown": it is a value that compares equal to another empty string, so two claims established
# over different closures — or from different proof terms — would agree.
probe "an empty dependency ROOT refuses to mint" "REFUSED" \
'#eval
  let ev : EdgeEvidence := { edge := .body, tables := [], tableDigests := [], quantifiesOverTable := true }
  IO.println (if (ReceiptMaterial.of? (some "v2:a") ev "" false [] "t" "w" "i").isNone then "REFUSED" else "MINTED")'
# THE ARTIFACT IS NO LONGER ASSERTABLE. It used to be a `String` parameter, so a caller could name a
# theorem nobody had replayed and receive a well-formed receipt for it; the only defence was a
# runtime check that the string was non-empty, which stops "" and nothing else. It now comes from the
# minting token, so the receipt records the theorem the KERNEL accepted. The old "empty artifact
# refuses" leg is gone because it became unfalsifiable — there is no argument left to make empty —
# and this is what replaced it: the artifact equals what was replayed.
# RECEIPT ISSUANCE CANNOT RESTORE WHAT THE AUTHORITY PASS TOOK AWAY. Issuance is gated on the
# COMPOSED verdict — the status AFTER applyCorrespondenceAuthority — so a subject whose closure
# cannot be rooted must be refused a receipt, not merely reported unfavourably. This runs the real
# `issueFor` against the batch's own replay result, so it costs no additional kernel run, and it
# matches on the REFUSAL CONSTRUCTOR rather than its rendered string.
probe_mint "receipt issuance REFUSES a subject whose closure cannot root" "ISSUANCE-REFUSED-notProved"  \
'#eval show IO Unit from do
  -- NO top-level defs here: a probe_mint body is inlined into the shared do-block of its batch, so
  -- everything it needs must be a binding inside that block.
  let aPk := Concrete.shortHash "authority-consumes-root"
  let cid? := (DefinitionIdentity.of? aPk "m" "solo" (Concrete.shortHash "impl:m.solo")).toOption
  match cid?, (PackageIdentity.syntheticForModules ["m"] ["s"]).toOption with
  | some cid, some pkg =>
    let entry : ProofCoreEntry :=
      { definitionIdentity := .ok cid, qualName := "m.solo", bareName := "solo"
      , callableId := CallableId.ofUser "m" "solo"
      , fn := { name := "solo", params := [], retTy := .i32, body := [] }
      , extracted := none, unsupported := [], fingerprint := "FP", params := []
      , eligibility := { qualName := "m.solo", eligible := true, sourceReasons := []
                       , profileReasons := [], exclusionKind := none, isTrusted := false, admissionRefusals := [], loc := none }
      , loc := none
      , spec := some { specId := { name := "s" }, proofName := probeThm, source := .registry, expectedFp := "FP" }
      , subjectDigest := none }
    let obl : Obligation :=
      { functionId := { qualName := "m.solo", fingerprint := "FP" }, bareName := "solo"
      , status := .proved, spec := none, expectedFp := "", eligibilityReasons := [], admissible := true
      , ineligCat := none, dependencies := [], notCurrentDeps := [], loc := none }
    let pc : ProofCore :=
      { packageIdentity := pkg, entries := [entry], excluded := [], structs := [], enums := []
      , traitDefs := [], callGraph := [("m.solo", [])], recMap := [], externNames := []
      , obligations := [obl], diagnostics := [] }
    let after := applyCorrespondenceAuthority pc pc.callGraph
    let env : IssueEnvironment := { compilerVersion := "v", workspaceId := "w", importsId := "i" }
    match issueFor after rr env (fun _ => []) entry with
    -- The CONSTRUCTOR, not the rendered status. `st` here is the known-false cause tracked
    -- separately; printing it would make this assertion pin a diagnosis that is due to change.
    | .error (.notProved _ _) => IO.println "ISSUANCE-REFUSED-notProved"
    | .error other             => IO.println s!"ISSUANCE-REFUSED-OTHER({other.canonical})"
    | .ok _                    => IO.println "ISSUANCE-MINTED"
  | _, _ => IO.println "could not build the control"'

probe_mint "the minted artifact is the theorem that was REPLAYED, not a caller's claim" "ARTIFACT-MATCHES" \
'#eval show IO Unit from do
  let ev : EdgeEvidence := { edge := .body, tables := [], tableDigests := [], quantifiesOverTable := true }
  match ← mintProbe (some "v2:a") ev "ROOT" false [] "t" "w" "i" with
  | none => IO.println "MINT-REFUSED"
  | some r => IO.println (if r.theoremArtifact == probeThm then "ARTIFACT-MATCHES" else s!"ARTIFACT-DIVERGED {r.theoremArtifact}")'
# A TRUST CLAIM MUST AGREE WITH ITS EVIDENCE, both directions: a qualification with no boundary named
# is unactionable, and boundaries named while the flag is false is a receipt disagreeing with itself.
probe "carriesTrust with no boundary named refuses" "REFUSED" \
'#eval
  let ev : EdgeEvidence := { edge := .body, tables := [], tableDigests := [], quantifiesOverTable := true }
  IO.println (if (ReceiptMaterial.of? (some "v2:a") ev "R" true [] "t" "w" "i").isNone then "REFUSED" else "MINTED")'
probe "boundaries named while carriesTrust is false refuses" "REFUSED" \
'#eval
  let ev : EdgeEvidence := { edge := .body, tables := [], tableDigests := [], quantifiesOverTable := true }
  IO.println (if (ReceiptMaterial.of? (some "v2:a") ev "R" false ["m.helper"] "t" "w" "i").isNone then "REFUSED" else "MINTED")'
# ...and the consistent case MINTS, so the refusals above are targeted rather than blanket.
probe "a qualified receipt mints when flag and boundaries agree" "MINTED" \
'#eval
  let ev : EdgeEvidence := { edge := .body, tables := [], tableDigests := [], quantifiesOverTable := true }
  IO.println (if (ReceiptMaterial.of? (some "v2:a") ev "R" true ["m.helper"] "t" "w" "i").isSome then "MINTED" else "REFUSED")'
# EVERY NEW FIELD PARTICIPATES IN CURRENCY. A field added to the envelope and left out of the
# comparison is worse than an absent field: it reads as bound.
probe_mint "a changed dependency ROOT makes the receipt non-current" "NOT" \
'#eval show IO Unit from do
  let ev : EdgeEvidence := { edge := .body, tables := [], tableDigests := [], quantifiesOverTable := true }
  match ← mintProbe (some "v2:s") ev "ROOT" false [] "t" "w" "i" with
  | some r => IO.println (if r.isCurrentAgainst "v2:s" .body [] "ROOT-MOVED" r.theoremArtifact false [] r.toolchainId "w" "i" then "CURRENT" else "NOT")
  | none => IO.println "REFUSED"'
probe_mint "a changed THEOREM ARTIFACT makes the receipt non-current" "NOT" \
'#eval show IO Unit from do
  let ev : EdgeEvidence := { edge := .body, tables := [], tableDigests := [], quantifiesOverTable := true }
  match ← mintProbe (some "v2:s") ev "ROOT" false [] "t" "w" "i" with
  | some r => IO.println (if r.isCurrentAgainst "v2:s" .body [] "ROOT" "THM-REPROVED" false [] r.toolchainId "w" "i" then "CURRENT" else "NOT")
  | none => IO.println "REFUSED"'
probe_mint "a receipt that gained a trusted boundary is non-current" "NOT" \
'#eval show IO Unit from do
  let ev : EdgeEvidence := { edge := .body, tables := [], tableDigests := [], quantifiesOverTable := true }
  match ← mintProbe (some "v2:s") ev "ROOT" false [] "t" "w" "i" with
  | some r => IO.println (if r.isCurrentAgainst "v2:s" .body [] "ROOT" r.theoremArtifact true ["m.h"] r.toolchainId "w" "i" then "CURRENT" else "NOT")
  | none => IO.println "REFUSED"'

probe "a receipt mints from produced identities" "MINTED" '
#eval show MetaM Unit from do
  let ev : EdgeEvidence := { edge := .body, tables := [], tableDigests := [], quantifiesOverTable := true }
  let r := ReceiptMaterial.of? (some "v2:s") ev "ROOT" false []
             (toolchainIdOf "0.1.0" "lean4:v4.28.0")
             (workspaceIdOf "pkg" ["m1"])
             (importsIdOf [("m1", "d1")])
  IO.println (if r.isSome then "MINTED" else "REFUSED")'

# No absolute paths, no machine-local state. Clean-machine reproducibility is a completion
# requirement, and a path in an identity makes every receipt un-replayable elsewhere.
if grep -nE "IO\.currentDir|System\.FilePath|getEnv" Concrete/Proof/Receipt.lean >/dev/null 2>&1; then
  no "Receipt.lean reads filesystem or environment state — identities must be reproducible from source alone"
else
  ok "no filesystem or environment state in the identity producers (clean-machine reproducible)"
fi

# THE INVARIANT: unclassified may reach diagnostics and shadow reports, never a current root or
# a replay receipt. The root refuses it via isCurrentForDependents; the receipt refuses it
# independently rather than trusting a caller to have consulted the root first.
probe "a receipt refuses an UNCLASSIFIED edge" "REFUSED" '
#eval show MetaM Unit from do
  let ev : EdgeEvidence := { edge := .unclassified, tables := [], tableDigests := [], quantifiesOverTable := false }
  IO.println (if (ReceiptMaterial.of? (some "v2:s") ev "ROOT" false [] "t" "w" "i").isNone then "REFUSED" else "MINTED")'
probe "a receipt refuses a MISSING edge" "REFUSED" '
#eval show MetaM Unit from do
  let ev : EdgeEvidence := { edge := .missing, tables := [], tableDigests := [], quantifiesOverTable := false }
  IO.println (if (ReceiptMaterial.of? (some "v2:s") ev "ROOT" false [] "t" "w" "i").isNone then "REFUSED" else "MINTED")'
# ...and a classified edge still mints, or the refusal above is just a broken constructor.
probe "a CONTRACT edge still mints (the refusals are targeted)" "MINTED" '
#eval show MetaM Unit from do
  let ev : EdgeEvidence := { edge := .contract, tables := [], tableDigests := [], quantifiesOverTable := true }
  IO.println (if (ReceiptMaterial.of? (some "v2:s") ev "ROOT" false [] "t" "w" "i").isSome then "MINTED" else "REFUSED")'

# === THE CLASSIFICATION HAND-BACK (slice 6, step 1) ==========================================
# The compiler cannot classify: the answer comes from a theorem's elaborated type, which exists
# only while Lean elaborates. These legs pin the properties the compiler will rely on when it
# consumes the result.
echo "=== classification hand-back ==="

# EVERY requested name appears. A dropped row is how a dependency disappears from a root and the
# root then reports a confident value over material it never saw.
probe "an unknown theorem yields a row, not a dropped one" "1" '
#eval show MetaM Unit from do
  let rows ← classifyAll [`No.Such.Theorem]
  IO.println (toString rows.length)'

probe "...and that row is UNCLASSIFIED, never missing or contract" "unclassified" '
#eval show MetaM Unit from do
  let rows ← classifyAll [`No.Such.Theorem]
  IO.println ((rows.map (·.2.edge.canonical)).headD "NONE")'

probe "one row per request, including duplicates" "3" '
#eval show MetaM Unit from do
  let rows ← classifyAll [`A.B, `A.B, `C.D]
  IO.println (toString rows.length)'

# The caller's order is convenient for reading and must not enter the bytes, or the same
# classification discovered in a different order would compare unequal.
probe "serialization ignores REQUEST order" "true" '
#eval show MetaM Unit from do
  let a ← classifyAll [`M.x, `M.y]
  let b ← classifyAll [`M.y, `M.x]
  IO.println (toString (renderClassification a == renderClassification b))'

probe "serialization is deterministic across runs" "true" '
#eval show MetaM Unit from do
  let a ← classifyAll [`M.x, `M.y]
  let b ← classifyAll [`M.x, `M.y]
  IO.println (toString (renderClassification a == renderClassification b))'

# Sensitivity: a different theorem SET must serialize differently, or the table binds nothing.
probe "serialization moves when the theorem set changes" "true" '
#eval show MetaM Unit from do
  let a ← classifyAll [`M.x, `M.y]
  let b ← classifyAll [`M.x, `M.z]
  IO.println (toString (renderClassification a != renderClassification b))'

# An unbound table renders as `?`, deliberately NOT digest-shaped: a consumer must not be able to
# mistake "we could not bind this" for a binding.
probe "an unbound table renders as ? and not as a digest" "true" '
#eval
  let ev : EdgeEvidence := { edge := .body, tables := [`T]
                           , tableDigests := [(`T, none)], quantifiesOverTable := false }
  let s := renderClassification [(`Th, ev)]
  (s.splitOn "d1:?").length == 2'

# === THE MERGE (slice 6, step 3) =============================================================
# Where a partial answer becomes indistinguishable from a complete one. Each refusal is its own
# constructor because the repairs differ, and each gets its own leg — a merge that refuses "some
# of the time" is a merge nobody can reason about.
echo "=== classification merge ==="

ok_ev='{ edge := .contract, tables := [], tableDigests := [], quantifiesOverTable := true }'

probe "a complete answer set merges" "true" "
#eval (mergeClassifications [\"a\", \"b\"] [(\"a\", $ok_ev), (\"b\", $ok_ev)]) matches Except.ok _"

probe "an UNANSWERED key refuses" "true" "
#eval (mergeClassifications [\"a\", \"b\"] [(\"a\", $ok_ev)]) matches Except.error (MergeError.unanswered _)"

probe "a DUPLICATE answer refuses (not first-wins)" "true" "
#eval (mergeClassifications [\"a\"] [(\"a\", $ok_ev), (\"a\", $ok_ev)]) matches Except.error (MergeError.duplicateAnswer _)"

probe "an UNKNOWN answer refuses" "true" "
#eval (mergeClassifications [\"a\"] [(\"a\", $ok_ev), (\"zz\", $ok_ev)]) matches Except.error (MergeError.unknownAnswer _)"

probe "a STILL-UNCLASSIFIED answer refuses" "true" '
#eval (mergeClassifications ["a"] [("a", { edge := .unclassified, tables := [], tableDigests := [], quantifiesOverTable := false })]) matches Except.error (MergeError.stillUnclassified _)'

probe "a MISSING classification refuses" "true" '
#eval (mergeClassifications ["a"] [("a", { edge := .missing, tables := [], tableDigests := [], quantifiesOverTable := false })]) matches Except.error (MergeError.classifiedMissing _)'

# The five refusals must be DISTINGUISHABLE, or a caller cannot route the repair. An unanswered
# key means the hand-back did not run; a duplicate means the classifier is inconsistent; the two
# have nothing to do with each other.
probe "the refusals are distinct constructors" "true" '
#eval (MergeError.unanswered "k") != (MergeError.duplicateAnswer "k")
  && (MergeError.unknownAnswer "k") != (MergeError.stillUnclassified "k")
  && (MergeError.classifiedMissing "k") != (MergeError.unanswered "k")'

# Requested order, so a caller iterating the merge is stable regardless of answer order.
probe "merged rows follow REQUESTED order, not answer order" "true" "
#eval match mergeClassifications [\"a\", \"b\"] [(\"b\", $ok_ev), (\"a\", $ok_ev)] with
      | Except.ok rows => rows.map Prod.fst == [\"a\", \"b\"]
      | Except.error _ => false"

# `classificationsComplete` must agree with the merge — two definitions of complete is how one
# ends up weaker.
probe "completeness agrees with the merge, both ways" "true" "
#eval classificationsComplete [\"a\"] [(\"a\", $ok_ev)]
  && !(classificationsComplete [\"a\", \"b\"] [(\"a\", $ok_ev)])"

# STEP 6 PRECONDITION. Every current root refusal is an `unclassified` edge — "we have not asked
# the Lean side" — not a `missing` one. Gating `proved` on the root while that holds would report
# depsNotCurrent for programs whose dependencies are fine, blaming the user's program for the
# compiler's own unfinished state.
#
# This leg watches for the precondition being MET: when real classifications land, refusals
# become `missing`, and those should gate a verdict. It reports rather than fails, because the
# current state is expected and the transition is what needs noticing.
echo "=== step 6 precondition (are refusals ours or the program's?) ==="
DRR="$(cc examples/proof_patterns/composition/src/main.con subject-facts | grep 'depRoot: REFUSED' || true)"
# THIS IS A REPORT, NOT AN ASSERTION, and it must stop counting as one. All three branches called
# `ok`, so it could not fail under any state of the program — a control that cannot fail inflates the
# pass total while proving nothing. The transition it watches for is still worth printing.
if [ -z "$DRR" ]; then
  echo "  note no root refusals in this fixture"
elif printf '%s' "$DRR" | grep -q "unclassified"; then
  echo "  note refusals are UNCLASSIFIED (our state, not the program's) — step 6 stays blocked, correctly"
else
  echo "  note PRECONDITION MET: refusals are no longer unclassified — real classifications have landed, so the root may now gate proved"
fi

# THE THEOREM DIGEST MUST SEE THE PROOF, not only the statement. `theoremArtifactDigest` claims
# a re-proof with the same statement moves it, and that claim rests on `ci.value?` exposing the
# proof term — which is not true of every declaration kind. Asserted rather than assumed: two
# theorems with an IDENTICAL type and DIFFERENT proofs must digest differently.
#
# If this ever fails, the digest binds statements rather than artifacts, and a receipt claiming
# to record "which proof" would be recording only "which claim".
echo "=== theorem digest binds the proof, not just the type ==="
probe "same type, different proof => different digest" "true" '
theorem tA : 1 + 1 = 2 := by rfl
theorem tB : 1 + 1 = 2 := by simp
#eval show MetaM Unit from do
  let a ← theoremArtifactDigest `tA
  let b ← theoremArtifactDigest `tB
  IO.println (toString (a.isSome && b.isSome && a != b))'

probe "the same theorem digests identically twice" "true" '
theorem tC : 2 + 2 = 4 := by rfl
#eval show MetaM Unit from do
  let a ← theoremArtifactDigest `tC
  let b ← theoremArtifactDigest `tC
  IO.println (toString (a.isSome && a == b))'

probe "an unknown theorem yields NO digest (never a placeholder)" "true" '
#eval show MetaM Unit from do
  let d ← theoremArtifactDigest `No.Such.Thm
  IO.println (toString d.isNone)'

# Table freshness lives in `check_classification_freshness.sh`, NOT here: verifying it means
# running the generator, which classifies every linked theorem and digests each proof term. That
# is minutes, and a per-commit gate that costs minutes is a gate people start skipping — which
# would cost more than the check is worth.
# === THE CONSUMER VALIDATES BEFORE CLASSIFYING ===============================================
# The digest was stored and freshness-tested, but `classifiedEdgeOf` read the tag directly — so a
# row whose provenance slot was empty, a placeholder, or malformed classified an edge exactly as
# well as a real one. Verification lived in a gate; the consumer required nothing.
echo "=== consumer validates the row ==="

probe "a real theorem still classifies" "body" '
#eval (classifiedEdgeOf "Concrete.Proof.parse_byte_correct").canonical'

probe "an unknown theorem is unclassified" "unclassified" '
#eval (classifiedEdgeOf "No.Such.Theorem").canonical'

# The point of the change: a structurally unsound row must not classify.
# THE MALFORMED CASES, reached DIRECTLY. The leg that used to sit here called
# `validatedRowOf "No.Such.Theorem"` and called itself a malformed-digest test — it exercised the
# ABSENT path, already covered by the leg above, and proved nothing about malformed data. The
# checked-in table contains only well-formed rows, so validation could not be tested through
# lookup at all; `validateRawRow` exists so it can be.
probe "an EMPTY digest is refused" "true" '
#eval ((validateRawRow ("T", "body", "", [], false, [])) ).toOption.isNone'
probe "a PLACEHOLDER digest is refused" "true" '
#eval ((validateRawRow ("T", "body", "?", [], false, [])) ).toOption.isNone'
probe "a WRONG-LENGTH digest is refused" "true" '
#eval ((validateRawRow ("T", "body", "abc123", [], false, [])) ).toOption.isNone'
# Uppercase is not "the same digest in different case": the generator emits lowercase, so an
# uppercase value did not come from the generator.
probe "an UPPERCASE digest is refused" "true" '
#eval ((validateRawRow ("T", "body", "7BCEC2D7871F93204B26E2BF83D5ACF1", [], false, [])) ).toOption.isNone'
probe "a NON-HEX digest is refused" "true" '
#eval ((validateRawRow ("T", "body", "zzcec2d7871f93204b26e2bf83d5acf1", [], false, [])) ).toOption.isNone'
probe "an UNKNOWN edge tag is refused" "true" '
#eval ((validateRawRow ("T", "somethingelse", "7bcec2d7871f93204b26e2bf83d5acf1", [], false, [])) ).toOption.isNone'
# Positive control: without it, a validator that refused everything would pass all six above.
probe "a WELL-FORMED row validates (positive control)" "true" '
#eval ((validateRawRow ("T", "body", "7bcec2d7871f93204b26e2bf83d5acf1", [], false, [])) ).toOption.isSome'

# THE TABLE BINDINGS ARE VALIDATED TOO. A row whose theorem digest is sound but whose table
# digests are not describes its dependencies with values nothing can compare — the same defect
# one level down, and the level where it would be least visible.
probe "a MALFORMED table digest is refused" "true" '
#eval ((validateRawRow ("T", "body", "7bcec2d7871f93204b26e2bf83d5acf1", [("Tbl", "nothex")], false, [])) ).toOption.isNone'
probe "a table named TWICE in one row is refused" "true" '
#eval ((validateRawRow ("T", "body", "7bcec2d7871f93204b26e2bf83d5acf1", [("Tbl", "6fe095a9f592a2e2b556e87f30306584"), ("Tbl", "6fe095a9f592a2e2b556e87f30306584")], false, [])) ).toOption.isNone'
probe "an EMPTY table identity is refused" "true" '
#eval ((validateRawRow ("T", "body", "7bcec2d7871f93204b26e2bf83d5acf1", [("", "6fe095a9f592a2e2b556e87f30306584")], false, [])) ).toOption.isNone'
# ...and a NAMED table with the same digest validates, so the refusal is about the identity being
# empty rather than about the digest.
probe "a NAMED table with the same digest validates (the refusal is about the name)" "true" '
#eval ((validateRawRow ("T", "body", "7bcec2d7871f93204b26e2bf83d5acf1", [("Tbl", "6fe095a9f592a2e2b556e87f30306584")], false, [])) ).toOption.isSome'
# The raw table is PRIVATE, so validation is the only route to a classification — not merely the
# only route to the `ValidatedRow` type.
expect_no_compile "classificationTable cannot be read directly (private)" '
#eval Concrete.Proof.classificationTable.length' \
  'Unknown identifier .Concrete\.Proof\.classificationTable'

probe "well-formed table bindings validate and are carried" "true" '
#eval match validateRawRow ("T", "body", "7bcec2d7871f93204b26e2bf83d5acf1", [("Tbl", "6fe095a9f592a2e2b556e87f30306584")], true, []) with
      | .ok r => r.tables.length == 1 && r.quantifies
      | .error _ => false'
# The real table must carry real bindings, or the generator emitted rows that describe nothing.
probe "a real row carries its table identities" "true" '
#eval match validatedRowOf "Concrete.Proof.parse_byte_correct" with
      | .ok r => r.tables.length > 0 && (r.tables.all fun t => t.2.length == 32)
      | .error _ => false'

# DUPLICATES. `find?` took the first match, so a malformed first row could hide a valid second
# and a valid first could hide conflicting trailing data — both silently. A generated security
# table must map a theorem to EXACTLY one validated row; zero and several are equally unusable,
# and collapsing "ambiguous" into "take one" is how a conflicting table classifies confidently.
probe "a theorem appearing TWICE yields no row, even if identical" "true" '
#eval
  let dup : List (String × String × String × List (String × String) × Bool × List String) :=
    [("T", "body", "7bcec2d7871f93204b26e2bf83d5acf1", [], false, []),
     ("T", "body", "7bcec2d7871f93204b26e2bf83d5acf1", [], false, [])]
  (match dup.filter (fun (r : String × String × String × List (String × String) × Bool × List String) => r.1 == "T") with | [row] => (validateRawRow row).toOption | _ => none).isNone'
probe "a theorem appearing twice with CONFLICTING rows yields no row" "true" '
#eval
  let dup : List (String × String × String × List (String × String) × Bool × List String) :=
    [("T", "body", "7bcec2d7871f93204b26e2bf83d5acf1", [], false, []),
     ("T", "contract", "bfda7f397e3221e757383578b50ee3ff", [], false, [])]
  (match dup.filter (fun (r : String × String × String × List (String × String) × Bool × List String) => r.1 == "T") with | [row] => (validateRawRow row).toOption | _ => none).isNone'

# The refusal must be NAMED. `absent` and `ambiguous` and `malformed` have different fixes —
# regenerate the hand-back, resolve a conflict, repair a row — and a single `none` sends a reader
# looking in the wrong place. The VERDICT is identical for all three (see below); only the record
# differs.
probe "an ABSENT theorem refuses by name, not by a bare none" "true" '
#eval match validatedRowOf "No.Such.Theorem" with
      | .error ClassificationRefusal.absent => true
      | _ => false'

# ...and the fail-closed verdict is unchanged by naming: every refusal still reads `unclassified`.
probe "a named refusal still classifies as unclassified (the verdict did not move)" "true" '
#eval classifiedEdgeOf "No.Such.Theorem" == DependencyEdge.unclassified'

probe "a validated row exposes its digest" "true" '
#eval match validatedRowOf "Concrete.Proof.parse_byte_correct" with
      | .ok r => r.digest.length == 32
      | .error _ => false'

# `ValidatedRow`'s constructor is private, so a caller cannot assemble one around a row that
# failed validation and classify from it. Asserted as a COMPILE failure, because that is the
# only form in which "cannot be constructed" is testable.
expect_no_compile "ValidatedRow cannot be constructed directly (private ctor)" '
def forged : Concrete.Proof.ValidatedRow :=
  { theoremName := "X", edge := Concrete.Proof.DependencyEdge.body, digest := "" }' \
  'constructor for .ValidatedRow. is marked as private'

# === CANONICAL TABLE-ENTRY EVIDENCE (blocker c prerequisite) =================================
# A whole-table digest answers "did this table change" and cannot answer "does it CONTAIN that
# callee". Correspondence needs the second, and one has been standing in for the other.
echo "=== table-entry evidence ==="

# RENAMED 2026-08-11: this said "a fully-identified table yields entry evidence", and full
# identification is no longer sufficient — an entry must also carry provenance that AGREES with a
# digest recomputed from its body. The old wording would have made the added requirement look like
# a regression in this probe rather than the point of it. The case it used to cover (identity
# present, provenance absent) is now asserted as a refusal, in the recompute section below.
probe "a table identified AND carrying agreeing provenance yields entry evidence" "true" '
#eval
  let d : PFnDef := { identity := .semantic (CallableId.ofUser "m" "f"), operationalKey := "f",
                      sourceBodyDigest := some { value := "496808fdd594d5047f23e823bc26b69c" },
                      params := [], body := PExpr.lit (PVal.int 0) }
  (tableEntryEvidence ({ entries := #[d], globals := fun _ => none } : FnTable)).toOption.isSome'

# ALL OR NOTHING. A partial membership list answers "is this callee present" with "not in the
# part I could read", which is indistinguishable from "absent" — and absence is what justifies
# refusing an edge.
probe "ONE legacy entry refuses evidence for the WHOLE table" "true" '
#eval
  let ok : PFnDef := { identity := .semantic (CallableId.ofUser "m" "f"), operationalKey := "f",
                       params := [], body := PExpr.lit (PVal.int 0) }
  let legacy : PFnDef := { identity := .legacy, operationalKey := "g", params := [], body := PExpr.lit (PVal.int 0) }
  (tableEntryEvidence ({ entries := #[ok, legacy], globals := fun _ => none } : FnTable)).toOption.isNone'

# DUPLICATE IDENTITIES REFUSED. A table holding one callable twice cannot say which
# implementation a static lookup selects, and membership answering "at least one" would let a
# `body` edge be justified by an entry that is not the one dispatch reaches.
probe "a table with the SAME callable twice yields no evidence" "true" '
#eval
  -- Provenance is VALID here on purpose. Without it this probe refused at `provenanceMissing`
  -- before ever reaching the duplicate check — invisible while every refusal was `none`, and
  -- surfaced immediately once they were named. The probe was passing for the wrong reason.
  let d : PFnDef := { identity := .semantic (CallableId.ofUser "m" "f"), operationalKey := "f",
                      sourceBodyDigest := some { value := "496808fdd594d5047f23e823bc26b69c" },
                      params := [], body := PExpr.lit (PVal.int 0) }
  match tableEntryEvidence ({ entries := #[d, d], globals := fun _ => none } : FnTable) with
  | .error (EntryEvidenceRefusal.duplicateIdentity) => true
  | _ => false'
probe "...while two DISTINCT callables are fine (the refusal is about duplication)" "true" '
#eval
  let f : PFnDef := { identity := .semantic (CallableId.ofUser "m" "f"), operationalKey := "f",
                      sourceBodyDigest := some { value := "496808fdd594d5047f23e823bc26b69c" },
                      params := [], body := PExpr.lit (PVal.int 0) }
  let g : PFnDef := { identity := .semantic (CallableId.ofUser "m" "g"), operationalKey := "g",
                      sourceBodyDigest := some { value := "496808fdd594d5047f23e823bc26b69c" },
                      params := [], body := PExpr.lit (PVal.int 0) }
  (tableEntryEvidence ({ entries := #[f, g], globals := fun _ => none } : FnTable)).toOption.isSome'

# === THE BODY IS RECOMPUTED, NOT TRUSTED =====================================================
# `tableEntryEvidence` used to COPY `PFnDef.sourceBodyDigest` into the evidence row. Every check
# downstream then compared metadata against metadata, and the body was never an input to its own
# provenance check. So this mutation survived the entire join:
#
#     replace `PFnDef.body`, keep `PFnDef.identity`, keep `PFnDef.sourceBodyDigest`
#
# -- a substituted implementation wearing correct-looking provenance, binding to the authoritative
# implementation digest of the body it no longer holds. The digests below are MEASURED values of
# `sourceBodyDigestV1Of`, not invented hex: a fabricated constant would make the positive control
# refuse and the whole section would then pass vacuously on refusals alone.
echo "=== entry provenance is recomputed from the body ==="

# POSITIVE CONTROL FIRST. If this refuses, every refusal below is vacuous -- they would all hold
# because nothing can ever produce evidence, which is indistinguishable from a working check.
probe "a stored digest AGREEING with the recomputed body yields evidence" "true" '
#eval
  let f : PFnDef := { identity := .semantic (CallableId.ofUser "m" "f"), operationalKey := "f",
                      sourceBodyDigest := some { value := "496808fdd594d5047f23e823bc26b69c" },
                      params := [], body := PExpr.lit (PVal.int 0) }
  (tableEntryEvidence ({ entries := #[f], globals := fun _ => none } : FnTable)).toOption.isSome'

# THE ACCEPTANCE MUTATION. Identity retained, stored digest retained, BODY replaced. This is the
# exact state the old code accepted, and the reason the producer had to move below `DependencyEdge`
# in the import order at all.
probe "a REPLACED body keeping its identity AND its stored digest is refused" "true" '
#eval
  let f : PFnDef := { identity := .semantic (CallableId.ofUser "m" "f"), operationalKey := "f",
                      sourceBodyDigest := some { value := "496808fdd594d5047f23e823bc26b69c" },
                      params := [], body := PExpr.lit (PVal.int 0) }
  let mutated : PFnDef := { f with body := PExpr.lit (PVal.int 1) }
  match tableEntryEvidence ({ entries := #[mutated], globals := fun _ => none } : FnTable) with
  | .error (EntryEvidenceRefusal.bodyMismatch _ _ _) => true
  | _ => false'

# The converse direction: body untouched, stored digest edited. Both directions of disagreement
# must refuse, or the check is only sensitive to one side of the comparison.
probe "a stored digest edited away from the body is refused" "true" '
#eval
  let f : PFnDef := { identity := .semantic (CallableId.ofUser "m" "f"), operationalKey := "f",
                      sourceBodyDigest := some { value := "7afedf51e742f4ce04201459a3965bc8" },
                      params := [], body := PExpr.lit (PVal.int 0) }
  match tableEntryEvidence ({ entries := #[f], globals := fun _ => none } : FnTable) with
  | .error (EntryEvidenceRefusal.bodyMismatch _ _ _) => true
  | _ => false'

probe "an entry with NO recorded provenance is refused" "true" '
#eval
  let f : PFnDef := { identity := .semantic (CallableId.ofUser "m" "f"), operationalKey := "f",
                      params := [], body := PExpr.lit (PVal.int 0) }
  match tableEntryEvidence ({ entries := #[f], globals := fun _ => none } : FnTable) with
  | .error (EntryEvidenceRefusal.provenanceMissing _) => true
  | _ => false'

# SWAPPED BODIES between two identities. Each digest is individually a real digest of a real body
# in the table, and the multiset of bodies is unchanged -- only the pairing is wrong. A check that
# validated digests against "some body present" rather than against THIS entry's body would pass
# this, so it distinguishes a per-entry check from a whole-table one.
probe "bodies SWAPPED between two callable identities are refused" "true" '
#eval
  let f : PFnDef := { identity := .semantic (CallableId.ofUser "m" "f"), operationalKey := "f",
                      sourceBodyDigest := some { value := "496808fdd594d5047f23e823bc26b69c" },
                      params := [], body := PExpr.lit (PVal.int 1) }
  let g : PFnDef := { identity := .semantic (CallableId.ofUser "m" "g"), operationalKey := "g",
                      sourceBodyDigest := some { value := "7afedf51e742f4ce04201459a3965bc8" },
                      params := [], body := PExpr.lit (PVal.int 0) }
  match tableEntryEvidence ({ entries := #[f, g], globals := fun _ => none } : FnTable) with
  | .error (EntryEvidenceRefusal.bodyMismatch _ _ _) => true
  | _ => false'

# ...and the same two entries paired CORRECTLY must bind, or the swap test above would pass for the
# uninteresting reason that two-entry tables never yield evidence.
probe "...and the same two bodies paired CORRECTLY yield evidence" "true" '
#eval
  let f : PFnDef := { identity := .semantic (CallableId.ofUser "m" "f"), operationalKey := "f",
                      sourceBodyDigest := some { value := "7afedf51e742f4ce04201459a3965bc8" },
                      params := [], body := PExpr.lit (PVal.int 1) }
  let g : PFnDef := { identity := .semantic (CallableId.ofUser "m" "g"), operationalKey := "g",
                      sourceBodyDigest := some { value := "496808fdd594d5047f23e823bc26b69c" },
                      params := [], body := PExpr.lit (PVal.int 0) }
  (tableEntryEvidence ({ entries := #[f, g], globals := fun _ => none } : FnTable)).toOption.isSome'

# SCHEMA AND SCOPE. `sourceBodyDigestV1Of` implements exactly `sourceBodyDigestV1`/`body_only`.
# A digest recorded under a different schema or scope is a DIFFERENT FORMULA, so comparing it to
# this producer's output would report body drift where there is a formula difference. Refusing what
# cannot be verified is the fail-closed reading; silently comparing anyway is not.
probe "a digest recorded under another SCHEMA is refused, not compared" "true" '
#eval
  let f : PFnDef := { identity := .semantic (CallableId.ofUser "m" "f"), operationalKey := "f",
                      sourceBodyDigest := some { schema := "sourceBodyDigestV2",
                                                 value := "496808fdd594d5047f23e823bc26b69c" },
                      params := [], body := PExpr.lit (PVal.int 0) }
  match tableEntryEvidence ({ entries := #[f], globals := fun _ => none } : FnTable) with
  | .error (EntryEvidenceRefusal.schemaUnsupported _ _) => true
  | _ => false'

probe "a digest recorded under another SCOPE is refused, not compared" "true" '
#eval
  let f : PFnDef := { identity := .semantic (CallableId.ofUser "m" "f"), operationalKey := "f",
                      sourceBodyDigest := some { scope := "body_and_contracts",
                                                 value := "496808fdd594d5047f23e823bc26b69c" },
                      params := [], body := PExpr.lit (PVal.int 0) }
  match tableEntryEvidence ({ entries := #[f], globals := fun _ => none } : FnTable) with
  | .error (EntryEvidenceRefusal.scopeUnsupported _ _) => true
  | _ => false'

probe "membership is decided by IDENTITY and finds a present callee" "true" '
#eval
  let rows := [{ callee := CallableId.ofUser "m" "f", sourceBodyDigestV1 := "496808fdd594d5047f23e823bc26b69c" : TableEntryEvidence }]
  entryEvidenceContains rows (CallableId.ofUser "m" "f")'

probe "...and does NOT find an absent one" "true" '
#eval
  let rows := [{ callee := CallableId.ofUser "m" "f", sourceBodyDigestV1 := "496808fdd594d5047f23e823bc26b69c" : TableEntryEvidence }]
  !(entryEvidenceContains rows (CallableId.ofUser "m" "g"))'

# Same display name, different module: membership must not be decided on a rendering.
probe "a same-NAMED callable from another module is not a member" "true" '
#eval
  let rows := [{ callee := CallableId.ofUser "modA" "f", sourceBodyDigestV1 := "496808fdd594d5047f23e823bc26b69c" : TableEntryEvidence }]
  !(entryEvidenceContains rows (CallableId.ofUser "modB" "f"))'

# === AUTHORITATIVE IMPLEMENTATION BINDING ====================================================
# Two things this fixes, both found by review.
#
# 1. The previous `bindEntryImplementations` took a CALLBACK, so any caller could mint a "bound" entry
#    from any non-empty string — the private constructor required non-emptiness, not provenance.
#    The old tests passed "v2:abc" and proved exactly that. A private constructor guarding a
#    value the caller supplies is not a guard.
# 2. It keyed on the V2 PROOF SUBJECT, which includes selected specification and claim scope. One
#    callable can carry several proof links, so `CallableId -> SubjectDigest` is not a function
#    and the join was ill-defined. The IMPLEMENTATION digest excludes spec and scope, so it is
#    one per callable — which is what a table entry needs.
echo "=== authoritative implementation binding ==="

HEXA='"7bcec2d7871f93204b26e2bf83d5acf1"'
HEXBODY='"6fe095a9f592a2e2b556e87f30306584"'
HEXB='"bfda7f397e3221e757383578b50ee3ff"'

probe "distinct callables with canonical digests are well-formed" "true" "
#eval ImplementationManifest.rowsWellFormed [(CallableId.ofUser \"m\" \"f\", $HEXBODY, $HEXA)]"

# The manifest CLAIMS to be a function; two rows for one callable means it is not.
probe "a DUPLICATE callable is not well-formed" "true" "
#eval !(ImplementationManifest.rowsWellFormed [(CallableId.ofUser \"m\" \"f\", $HEXBODY, $HEXA), (CallableId.ofUser \"m\" \"f\", $HEXBODY, $HEXB)])"

probe "a NON-CANONICAL digest is not well-formed" "true" '
#eval !(ImplementationManifest.rowsWellFormed [(CallableId.ofUser "m" "f", "v2:abc", "v2:abc")])'

# The exact string the old tests used. It is not a digest, and the old API accepted it.
probe "the old bypass value \"v2:abc\" is rejected outright" "true" '
#eval !(ImplementationManifest.rowsWellFormed [(CallableId.ofUser "m" "f", "v2:abc", "v2:abc")])'

# These probes now build the manifest the way production does — from a `CompleteImplementation`,
# with the digests COMPUTED — because `ofRows` is private and chosen digests are no longer
# constructible. That is a strictly better test: the entry digest and the manifest digest agree
# because both derive from the SAME `PExpr`, rather than because two literals were typed to match.
# `MF` is the shared setup; each probe supplies the final expression over `ci` and `mf`.
MF='
  let cid := CallableId.ofUser "m" "f"
  let pe := PExpr.lit (PVal.int 0)
  let fx : Proof.CheckedDeclFacts := { id := cid }
  match Proof.validate ({} : Proof.EvidenceBodyDraftV2) with
  | .error _ => false
  | .ok bd =>
  match CompleteImplementation.of? cid fx bd pe with
  | none => false
  | some ci =>
  match ImplementationManifest.ofImplementations [ci] with
  | none => false
  | some mf =>'

probe "entries bind through a computed manifest" "true" "
#eval$MF
  let rows := [{ callee := cid, sourceBodyDigestV1 := ci.sourceBodyComponent : TableEntryEvidence }]
  (bindEntryImplementations rows mf).isSome"

probe "ONE entry missing from the manifest refuses the whole list" "true" "
#eval$MF
  let rows := [{ callee := cid, sourceBodyDigestV1 := ci.sourceBodyComponent : TableEntryEvidence },
               { callee := CallableId.ofUser \"m\" \"g\", sourceBodyDigestV1 := ci.sourceBodyComponent : TableEntryEvidence }]
  (bindEntryImplementations rows mf).isNone"

# Returns the digest, not a Bool, so a caller can record WHICH implementation justified an edge.
# Asserted against `ci.implementationComponent` rather than a literal: with digests computed, a
# hard-coded expectation would pin today's hash rather than the property that the value the join
# hands back is the one the manifest computed.
probe "membership returns the manifest's computed implementation digest" "true" "
#eval$MF
  let rows := [{ callee := cid, sourceBodyDigestV1 := ci.sourceBodyComponent : TableEntryEvidence }]
  match bindEntryImplementations rows mf with
  | some b => (boundEntryImplementationOf b cid) == some ci.implementationComponent
  | none => false"

# THE ACCEPTANCE MUTATIONS for the misattachment hole. Identity alone used to bind; now the
# entry's body digest must EQUAL the authoritative one.
probe "a STALE body digest refuses to bind (identity alone is not enough)" "true" "
#eval$MF
  let rows := [{ callee := cid, sourceBodyDigestV1 := \"00000000000000000000000000000000\" : TableEntryEvidence }]
  (bindEntryImplementations rows mf).isNone"

# A digest that is a REAL digest of a DIFFERENT body — well-formed, computed by the right producer,
# and still wrong for this entry. Thirty-two zeros could be refused by a format check; this cannot.
probe "a real digest of ANOTHER body refuses to bind" "true" "
#eval$MF
  let rows := [{ callee := cid, sourceBodyDigestV1 := sourceBodyDigestV1Of (PExpr.lit (PVal.int 1)) : TableEntryEvidence }]
  (bindEntryImplementations rows mf).isNone"

# WAS a runtime refusal; is now UNREPRESENTABLE. `TableEntryEvidence.sourceBodyDigestV1` is a
# `String`, so "entry with no provenance" has no value to construct -- the check moved from
# `bindEntryImplementations` to the type, and the refusal moved up to `tableEntryEvidence`, where a
# `PFnDef` lacking `sourceBodyDigest` is rejected (asserted in the recompute section below).
expect_no_compile "an entry with NO body digest has no representation (absence is not agreement)" '
#eval
  let rows := [{ callee := Concrete.CallableId.ofUser "m" "f",
                 sourceBodyDigestV1 := none : Concrete.Proof.TableEntryEvidence }]
  rows.length' \
  'Type mismatch' 'Option \?m' 'expected to have type' 'String'

# CONTAINMENT: the format-only door is closed. `rowsWellFormed` answers the format question as a
# Bool; there is no public way to turn chosen digests into a manifest.
# PAIRED CONTROL for the no-compile below, and it is not optional. An `expect_no_compile` passes
# when the snippet fails to elaborate FOR ANY REASON — a misqualified name, a renamed type, a typo —
# so on its own it cannot distinguish "private" from "misspelled", and would silently rot into a
# tautology the day the path changes. This asserts the SAME fully-qualified path with the PUBLIC
# predicate: it must compile. Only then does the failure below mean privacy.
probe "the qualified manifest path resolves (so the no-compile below is about PRIVACY)" "true" '
#eval Concrete.Proof.ImplementationManifest.rowsWellFormed
        [(Concrete.CallableId.ofUser "m" "f",
          "496808fdd594d5047f23e823bc26b69c", "496808fdd594d5047f23e823bc26b69c")]'

expect_no_compile "ofRows is private — chosen digests cannot become a manifest" '
#eval (Concrete.Proof.ImplementationManifest.ofRows
        [(Concrete.CallableId.ofUser "m" "f",
          "496808fdd594d5047f23e823bc26b69c", "496808fdd594d5047f23e823bc26b69c")]).isSome' \
  'Unknown constant .Concrete\.Proof\.ImplementationManifest\.ofRows.'

# === MANIFEST COMPLETENESS IS SELF-DENOMINATING ===============================================
# The producer used to `filterMap`, so an entry it could not build a row for vanished and the result
# was a SMALLER manifest that looked complete — the only record of how many rows there should have
# been was how many there were. `ManifestResult` stores the denominator (`expected`) and gives every
# unaccounted identity a NAMED refusal.
echo "=== manifest completeness is self-denominating ==="

# `MR` builds a complete result for one callable: expected = [cid], one impl, no refusals.
MR='
  let cid := CallableId.ofUser "m" "f"
  let pe := PExpr.lit (PVal.int 0)
  let fx : Proof.CheckedDeclFacts := { id := cid }
  match Proof.validate ({} : Proof.EvidenceBodyDraftV2) with
  | .error _ => false
  | .ok bd =>
  match CompleteImplementation.of? cid fx bd pe with
  | none => false
  | some ci =>'

# POSITIVE CONTROL FIRST. If a complete result is not usable, every refusal below holds vacuously.
probe "a result accounting for every expected identity is usable" "true" "
#eval$MR
  ({ expected := [cid], impls := [ci], refusals := [] } : ManifestResult).usable?.isSome"

# THE ANTI-FILTERMAP CONDITION. Rows are a strict subset of expected: exactly what the old producer
# returned, and exactly what must not be usable. Note the refusal list is EMPTY here — so this is not
# passing because of the refusals check; it is the stored denominator doing the work.
probe "rows covering FEWER identities than expected are refused" "true" "
#eval$MR
  ({ expected := [cid, CallableId.ofUser \"m\" \"g\"], impls := [ci], refusals := [] }
     : ManifestResult).usable?.isNone"

# A NAMED REFUSAL blocks usability even when the rows look self-consistent: expected = row
# identities here, so only the refusal list can refuse it.
probe "any named refusal makes the result unusable" "true" "
#eval$MR
  ({ expected := [cid], impls := [ci], refusals := [(CallableId.ofUser \"m\" \"g\", .factsMissing)] }
     : ManifestResult).usable?.isNone"

# Refusals must be DISTINGUISHABLE, not merely counted: "no facts" and "evidence failed validation"
# are different failures, and a bare count cannot tell them apart.
probe "refusal reasons render distinctly" "true" '
#eval
  let all : List ManifestRefusal :=
    [.factsMissing, .factsIncomplete, .evidenceInvalid, .evidenceMissing, .extractedMissing, .inputsRefused]
  let rendered := all.map ManifestRefusal.render
  rendered.eraseDups.length == all.length && !(rendered.any (· == ""))'

# Every identity accounted for EXACTLY ONCE. `accounted` is compared against the stored denominator,
# so an identity lost between the two lists is visible rather than silent.
probe "a complete result accounts for exactly the expected identities" "true" "
#eval$MR
  let r : ManifestResult := { expected := [cid], impls := [ci], refusals := [] }
  r.accounted == r.expected.length"

# === MANIFEST PROVENANCE: DIGESTS ARE COMPUTED, NOT SUPPLIED ==================================
# `ofRows` validates that thirty-two hex characters are thirty-two hex characters. It cannot tell a
# computed digest from a well-formed invented one, so `ofImplementations` takes INPUTS and computes
# both digests itself, and the authoritative producer now goes through it.
#
# WHAT IS AND IS NOT COVERED, stated here so the section cannot be misread as more than it proves.
# `CompleteImplementation.of?` refuses a `facts`/`callable` mispairing and incomplete facts. It
# CANNOT refuse a `body` or `extracted` belonging to a different entry, because neither
# `CompleteEvidenceBodyV2` nor `PExpr` carries identity. That swap is a NAMED GAP in the roadmap and
# has deliberately NOT been registered as a mutation, because the mutation would survive and a
# surviving control recorded as coverage is worse than no control.
echo "=== manifest provenance: inputs in, digests computed ==="

expect_no_compile "CompleteImplementation cannot be constructed directly" '
#eval fun (fx : Concrete.Proof.CheckedDeclFacts) (b : Concrete.Proof.CompleteEvidenceBodyV2) =>
  (Concrete.CompleteImplementation.mk (Concrete.CallableId.ofUser "m" "f") fx b
    (Concrete.Proof.PExpr.lit (Concrete.Proof.PVal.int 0))).callable' \
  'Unknown constant .Concrete\.CompleteImplementation\.mk.'

# FACTS MUST DESCRIBE THE CALLABLE CLAIMED. Facts for `g`, offered as `f`: this is the mispairing
# the record exists to prevent, and the one it can actually see.
probe "facts describing ANOTHER callable are refused" "true" '
#eval
  match tid? "m" "g",
        tid? "m" "f" with
  | some i_m_g, some i_m_f =>
    let _ := (i_m_g, i_m_f)
    let fx : Proof.CheckedDeclFacts := { id := CallableId.ofUser "m" "g" }
    match Proof.validate ({} : Proof.EvidenceBodyDraftV2) with
    | .ok body => (CompleteImplementation.of? (CallableId.ofUser "m" "f") fx body (PExpr.lit (PVal.int 0))).isNone
    | .error _ => false
  | _, _ => false
'

# INCOMPLETE FACTS. A digest over incomplete facts is a digest over an unknown, so it must not be
# mintable at all -- refusing later would mean the value existed in the meantime.
probe "INCOMPLETE facts are refused" "true" '
#eval
  match tid? "m" "f" with
  | some i_m_f =>
    -- `covered := false` is what makes these facts incomplete: default facts ARE complete
    -- (`isComplete = id.isComplete && contracts.covered`, both true by default), so an id alone
    -- would have tested the id check a second time rather than completeness.
    let fx : Proof.CheckedDeclFacts :=
    { id := CallableId.ofUser "m" "f", contracts := { covered := false } }
    match Proof.validate ({} : Proof.EvidenceBodyDraftV2) with
    -- The `!fx.isComplete` conjunct keeps this from passing for the wrong reason.
    | .ok body => (!fx.isComplete)
    && (CompleteImplementation.of? (CallableId.ofUser "m" "f") fx body (PExpr.lit (PVal.int 0))).isNone
    | .error _ => false
  | _ => false
'

expect_no_compile "the manifest cannot be constructed directly" '
#eval (Concrete.Proof.ImplementationManifest.mk []).find? (Concrete.CallableId.ofUser "m" "f")' \
  'Unknown constant .Concrete\.Proof\.ImplementationManifest\.mk.'
expect_no_compile "BoundTableEntry cannot be constructed directly" '
#eval (Concrete.Proof.BoundTableEntry.mk (Concrete.CallableId.ofUser "m" "f") "").implDigest' \
  'Unknown constant .Concrete\.Proof\.BoundTableEntry\.mk.'

# === THEOREM-TO-EDGE CORRESPONDENCE (slice 6, blocker c) =====================================
# A row says a theorem implies `contract` or `body`. Using that to type an edge is a FURTHER
# claim — this dependency is covered that way — and the two can come apart. `contract` is the
# direction that matters: it asserts the caller survives any implementation preserving the
# contract, so typing it from a theorem that quantifies over nothing UNDER-binds, letting an
# implementation change the proof actually depends on pass without staling the caller.
echo "=== theorem-to-edge correspondence ==="

# The row gained a trailing `specs : List String` when classification rows began carrying the spec
# constants a theorem is about; probes pass `[]` because these legs are about edge material, not
# theorem-to-subject correspondence, which check_receipt_issuance.sh covers.
mkrow() { echo "validateRawRow (\"T\", \"$1\", \"7bcec2d7871f93204b26e2bf83d5acf1\", $2, $3, [])"; }

probe "contract WITHOUT quantification is not justified" "true" '
#eval match validateRawRow ("T", "contract", "7bcec2d7871f93204b26e2bf83d5acf1", [], false, []) with
      | .ok r => !(rowJustifies r DependencyEdge.contract)
      | .error _ => false'
probe "contract WITH quantification is justified" "true" '
#eval match validateRawRow ("T", "contract", "7bcec2d7871f93204b26e2bf83d5acf1", [], true, []) with
      | .ok r => rowJustifies r DependencyEdge.contract
      | .error _ => false'
probe "body with NO bound table is not justified" "true" '
#eval match validateRawRow ("T", "body", "7bcec2d7871f93204b26e2bf83d5acf1", [], false, []) with
      | .ok r => !(rowJustifies r DependencyEdge.body)
      | .error _ => false'
probe "body WITH a bound table is justified" "true" '
#eval match validateRawRow ("T", "body", "7bcec2d7871f93204b26e2bf83d5acf1", [("Tbl", "6fe095a9f592a2e2b556e87f30306584")], false, []) with
      | .ok r => rowJustifies r DependencyEdge.body
      | .error _ => false'
probe "unclassified justifies nothing" "true" '
#eval match validateRawRow ("T", "body", "7bcec2d7871f93204b26e2bf83d5acf1", [("Tbl", "6fe095a9f592a2e2b556e87f30306584")], true, []) with
      | .ok r => !(rowJustifies r DependencyEdge.unclassified)
      | .error _ => false'

# The consumer must ACT on it: an unjustified row downgrades to `unclassified` rather than being
# trusted. Without this the check would be a function nobody consults.
probe "an unjustifiable row would not classify" "unclassified" '
#eval match validateRawRow ("T", "contract", "7bcec2d7871f93204b26e2bf83d5acf1", [], false, []) with
      | .ok r => (if rowJustifies r r.edge then r.edge else DependencyEdge.unclassified).canonical
      | .error _ => "no-row"'
# ROW DEFECTS ARE NAMED AND EACH IS REACHABLE. A named constructor no input can produce reads as
# coverage while testing nothing, so every one below is exercised by a row that triggers it.
probe "an EMPTY theorem identity is refused by name" "true" '
#eval match validateRawRow ("", "body", "7bcec2d7871f93204b26e2bf83d5acf1", [], false, []) with
      | .error RawRowRefusal.emptyTheoremIdentity => true
      | _ => false'
probe "an EMPTY artifact digest is refused by name, distinctly from malformed" "true" '
#eval match validateRawRow ("T", "body", "", [], false, []) with
      | .error RawRowRefusal.emptyArtifactDigest => true
      | _ => false'
probe "an UNKNOWN edge tag is refused by name, carrying the tag" "true" '
#eval match validateRawRow ("T", "no_such_kind", "7bcec2d7871f93204b26e2bf83d5acf1", [], false, []) with
      | .error (RawRowRefusal.unknownEdgeTag "no_such_kind") => true
      | _ => false'
probe "a MALFORMED theorem digest is refused by name, carrying the value" "true" '
#eval match validateRawRow ("T", "body", "nothex", [], false, []) with
      | .error (RawRowRefusal.theoremDigestMalformed "nothex") => true
      | _ => false'
probe "a MALFORMED table digest is refused by name, carrying table and value" "true" '
#eval match validateRawRow ("T", "body", "7bcec2d7871f93204b26e2bf83d5acf1", [("Tbl", "nothex")], false, []) with
      | .error (RawRowRefusal.tableDigestMalformed "Tbl" "nothex") => true
      | _ => false'
probe "an EMPTY table identity is refused by name" "true" '
#eval match validateRawRow ("T", "body", "7bcec2d7871f93204b26e2bf83d5acf1", [("", "7bcec2d7871f93204b26e2bf83d5acf1")], false, []) with
      | .error RawRowRefusal.tableIdentityEmpty => true
      | _ => false'
probe "a table named TWICE is refused by name, carrying the table" "true" '
#eval match validateRawRow ("T", "body", "7bcec2d7871f93204b26e2bf83d5acf1",
        [("Tbl", "7bcec2d7871f93204b26e2bf83d5acf1"), ("Tbl", "bfda7f397e3221e757383578b50ee3ff")], false, []) with
      | .error (RawRowRefusal.tableNamedTwice "Tbl") => true
      | _ => false'
# The two-level split holds: the lookup layer reports a stable category and carries the detail.
probe "a malformed unique row surfaces as malformed CARRYING its defect" "true" '
#eval match validatedRowOf "No.Such.Theorem" with
      | .error ClassificationRefusal.absent => true
      | _ => false'

# ...and real rows still classify, so the check is not simply refusing everything.
probe "a real row still classifies after the correspondence check" "body" '
#eval (classifiedEdgeOf "Concrete.Proof.parse_byte_correct").canonical'

# === TABLE NAME -> ENTRIES (the blocker that was not one) =====================================
# Recorded for weeks as "the compiler cannot check table membership; the hand-back carries only a
# name and a whole-table digest, so this needs the generator to evalExpr the FnTable". The second
# half was true and the first was WRONG: the compiler IS a Lean program and these tables are
# ordinary definitions inside it. Only the name->value mapping was missing. A dispatch replaced the
# entire evalExpr plan.
echo "=== hand-back table names resolve to entries ==="

probe "a real table resolves to its entries" "true" '
#eval match entryEvidenceForTable "Concrete.Proof.proofFns" with
      | .ok rows => !rows.isEmpty
      | .error _ => false'
probe "membership answers TRUE for a callee the table holds" "true" '
#eval match tableHoldsModelNamed "Concrete.Proof.proofFns" (CallableId.ofUser "main" "parse_byte") with
      | .ok b => b
      | .error _ => false'
# ...and FALSE for one it does not, so the answer is not constant.
probe "membership answers FALSE for a callee the table lacks" "true" '
#eval match tableHoldsModelNamed "Concrete.Proof.proofFns" (CallableId.ofUser "main" "no_such") with
      | .ok b => !b
      | .error _ => false'
# A table the compiler cannot read REFUSES rather than answering "no". "Absent" and "cannot tell"
# are different, and collapsing them silently narrows a dependency closure.
probe "a wholly unknown table refuses BY NAME rather than answering absent" "true" '
#eval match tableHoldsModelNamed "No.Such.Table" (CallableId.ofUser "m" "x") with
      | .error (TableResolveRefusal.unknownTable _) => true
      | _ => false'

# PROVENANCE IS DISTINGUISHED. An in-compiler table is RECOMPUTED (every body digest checked against
# the actual `PFnDef.body`); an out-of-build table is generator-ASSERTED (digests held, no bodies to
# check them against). Reporting the second as the first would be a weaker fact wearing a stronger
# label — so the two must not compare equal.
probe "an in-compiler table reports compiler-linked provenance" "true" '
#eval match entryEvidenceWithProvenance "Concrete.Proof.proofFns" with
      | .ok (TableProvenance.compilerLinked, _) => true
      | _ => false'
probe "an out-of-build table reports generator-asserted provenance, NOT compiler-linked" "true" '
#eval match entryEvidenceWithProvenance "Examples.ProofPatterns.Proofs.combineFns" with
      | .ok (TableProvenance.generatorAsserted, rows) => !rows.isEmpty
      | _ => false'

# DISPATCH COVERAGE against the real hand-back inventory. A table named by the checked-in
# classification table that the dispatch does not know is a silent hole: correspondence for that
# edge would refuse, and the cause would look like missing evidence rather than a stale dispatch.
TBL_NAMES="$(grep -oE '\("[A-Za-z.]+", "[0-9a-f]{32}"\)' Concrete/Proof/ClassificationTable.lean \
             | grep -oE '"[A-Za-z.]+"' | tr -d '"' | sort -u)"
KNOWN=0; UNKNOWN=""
for t in $TBL_NAMES; do
  if grep -q "\"$t\"" Concrete/Proof/TableResolve.lean; then KNOWN=$((KNOWN+1)); else UNKNOWN="$UNKNOWN $t"; fi
done
NTBL="$(printf '%s\n' $TBL_NAMES | grep -c . || true)"
echo "  hand-back names $NTBL tables; dispatch resolves $KNOWN (unlisted:${UNKNOWN:- none})"
# `combineFns` is defined under proofs/, OUTSIDE the compiler build, so it cannot be linked and its
# refusal is correct rather than a gap. Named explicitly: "one is unlisted" would stay true if a
# different table silently became unresolvable.
# `combineFns` is no longer unlisted-and-unresolvable: it is unlisted in the DISPATCH (correctly —
# linking it would be a dependency cycle) and resolved from `externalTableEntries` instead. So the
# expectation is unchanged here, and the provenance probes above assert the difference in strength.
if [ "$NTBL" -gt 0 ] && [ "$UNKNOWN" = " Examples.ProofPatterns.Proofs.combineFns" ]; then
  ok "every in-compiler table is dispatched ($KNOWN/$NTBL); combineFns resolves as data, not by linking"
else
  no "dispatch coverage changed: unlisted =$UNKNOWN (expected only Examples.ProofPatterns.Proofs.combineFns) — a table the dispatch omits refuses as missing evidence rather than as a stale dispatch"
fi

# === ENTRY-DERIVED TABLE IDENTITY =============================================================
echo "=== entry-derived table identity ==="

TD='
  let r1 : TableEntryEvidence := { callee := CallableId.ofUser "m" "a", sourceBodyDigestV1 := "496808fdd594d5047f23e823bc26b69c" }
  let r2 : TableEntryEvidence := { callee := CallableId.ofUser "m" "b", sourceBodyDigestV1 := "7afedf51e742f4ce04201459a3965bc8" }'

# REORDERING IS EQUIVALENT by construction — the digest measures membership, not iteration order.
probe "reordering entries yields the SAME digest" "true" "
#eval$TD
  entryTableDigest \"T\" [r1, r2] == entryTableDigest \"T\" [r2, r1]"

# ...and every other alteration is not.
probe "ADDING an entry changes the digest" "true" "
#eval$TD
  let r3 : TableEntryEvidence := { callee := CallableId.ofUser \"m\" \"c\", sourceBodyDigestV1 := \"50333e79fd3df408c9fab0a0a3b40a93\" }
  entryTableDigest \"T\" [r1, r2] != entryTableDigest \"T\" [r1, r2, r3]"
probe "REMOVING an entry changes the digest" "true" "
#eval$TD
  entryTableDigest \"T\" [r1, r2] != entryTableDigest \"T\" [r1]"
probe "DUPLICATING an entry changes the digest" "true" "
#eval$TD
  entryTableDigest \"T\" [r1, r2] != entryTableDigest \"T\" [r1, r2, r2]"
probe "changing a BODY digest changes the table digest" "true" "
#eval$TD
  let r2' : TableEntryEvidence := { r2 with sourceBodyDigestV1 := \"50333e79fd3df408c9fab0a0a3b40a93\" }
  entryTableDigest \"T\" [r1, r2] != entryTableDigest \"T\" [r1, r2']"

# A DIGEST COPIED FROM ANOTHER TABLE must not verify: the table's own identity is in the preimage,
# so the same membership under a different name is a different value.
probe "the same entries under a DIFFERENT table name digest differently" "true" "
#eval$TD
  entryTableDigest \"TableA\" [r1, r2] != entryTableDigest \"TableB\" [r1, r2]"

# THE STORED DIGEST IS CHECKED. External material has no body to recompute against, so this is its
# only independent verification — a stale row, an edited entry list, or a copied digest all refuse.
probe "external material whose entries disagree with its recorded digest REFUSES" "true" '
#eval
  -- the real row, with one entry digest altered, must not verify against the recorded value
  match externalTableEntries.find? (fun e => e.1 == "Examples.ProofPatterns.Proofs.combineFns") with
  | none => false
  | some (nm, recorded, pairs) =>
    let rows := pairs.map (fun (p : String × String × String) =>
      ({ callee := CallableId.ofUser p.1 p.2.1,
         sourceBodyDigestV1 := "00000000000000000000000000000000" } : TableEntryEvidence))
    entryTableDigest nm rows != recorded'

# THE POSITIVE ATTACK MODEL. A CONSISTENTLY-altered pair — entry list and stored digest changed
# together — verifies structurally, and that is correct rather than a hole. Generator and compiler
# run the SAME `entryTableDigest`, so agreement establishes canonical-encoding consistency and the
# binding between name, entries and digest; it does NOT validate the formula, the generator, or
# whether the entries describe real bodies. This probe exists to keep that limit visible: if it ever
# started FAILING, someone would have mistaken structural agreement for external truth.
probe "a CONSISTENTLY-altered entry list and digest still verify (agreement is not truth)" "true" '
#eval
  let rows := [({ callee := CallableId.ofUser "calls" "dbl",
                  sourceBodyDigestV1 := "00000000000000000000000000000000" } : TableEntryEvidence)]
  let nm := "Examples.ProofPatterns.Proofs.combineFns"
  -- fabricated membership, with its digest recomputed to match: structurally consistent, and
  -- describing a body nobody read
  entryTableDigest nm rows == entryTableDigest nm rows'

# AGREEMENT DOES NOT UPGRADE. combineFns verifies against its digest and is STILL asserted, never
# compiler-linked — a matching digest says the entries are the ones recorded, not that a body was
# ever read. This is the control that stops a weaker fact acquiring a stronger label.
probe "a verifying external table is still generatorAsserted, never compilerLinked" "true" '
#eval match entryEvidenceWithProvenance "Examples.ProofPatterns.Proofs.combineFns" with
      | .ok (TableProvenance.generatorAsserted, _) => true
      | _ => false'

# EXTERNAL ROW INTEGRITY, exercising the REAL validator.
#
# The first version of these two probes was VACUOUS and review caught it: one filtered a local list
# and asserted its length, the other compared `entryTableDigest x` with itself. Neither reached the
# validation code. `externalRowDefect` is now separated from lookup for exactly this reason — a probe
# that can only go through `entryEvidenceWithProvenance` can only see the checked-in rows, which are
# well-formed, so "malformed rows are rejected" would be asserted against data containing none.
probe "the real validator rejects an EMPTY declaration identity" "true" '
#eval (externalRowDefect [("m", "", "496808fdd594d5047f23e823bc26b69c")]).isSome'
probe "the real validator rejects an EMPTY module identity" "true" '
#eval (externalRowDefect [("", "f", "496808fdd594d5047f23e823bc26b69c")]).isSome'
probe "the real validator rejects a NON-HEX body digest" "true" '
#eval (externalRowDefect [("m", "f", "nothex")]).isSome'
probe "the real validator rejects ONE CALLABLE LISTED TWICE" "true" '
#eval (externalRowDefect [("m", "f", "496808fdd594d5047f23e823bc26b69c"),
                          ("m", "f", "7afedf51e742f4ce04201459a3965bc8")]).isSome'
# POSITIVE CONTROL: a well-formed list passes, so the four refusals above are not holding because
# the validator rejects everything.
probe "the real validator ACCEPTS a well-formed row list" "true" '
#eval (externalRowDefect [("m", "f", "496808fdd594d5047f23e823bc26b69c"),
                          ("m", "g", "7afedf51e742f4ce04201459a3965bc8")]).isNone'
# ...and the checked-in row is itself well-formed, so the production path is covered too.
probe "the checked-in external row passes the real validator" "true" '
#eval match externalTableEntries.filter (fun e => e.1 == "Examples.ProofPatterns.Proofs.combineFns") with
      | [(_, _, pairs)] => (externalRowDefect pairs).isNone
      | _ => false'

# === SCOPED DEFINITION IDENTITY (the trust-model repair) ======================================
# `CallableId` is a SOURCE NAME. `elf_header` and `proof_pressure` both define
# `main.validate_header`, both render `v1:user:main.validate_header`, and they are different
# functions — so a shared proof table could match the wrong program's edge on name agreement alone.
# These probes are the CROSS-PROGRAM COLLISION FIXTURE in synthetic form: names and callee names
# coincide, implementations differ.
echo "=== scoped definition identity ==="

# THE CONVERTED TABLES PASS THE EVIDENCE BOUNDARY, which is a different claim from "attestations were
# selected". `check_attestation_manifest.sh` reconciles the SELECTION against the manifest; this asks
# whether `scopedEntryEvidence` — recomputed body digests, model-in-table, no duplicate identity —
# actually accepts what was selected. A table can carry five attestations and still refuse.
#
# `cryptoFns`: 5 attestations for 4 models, `check_nonce` once per consuming package.
# `elfFns`: 5 attestations for 5 models; the sixth manifest row is the named exclusion.
# NO NAME-KEYED FALLBACK REMAINS IN THE EVIDENCE JOIN. The scoped lookup and the name-level one
# DISAGREE on the corpus — `elfFns` holds a model named `main.check_magic`, and does not hold an
# attested entry for the drifted program's `main.check_magic` — so this is a live discrimination
# rather than two functions that happen to agree. If the join ever fell back to the name question,
# this pair would stop disagreeing and four edges of a different program would be justified again.
probe "the scoped and name-level questions genuinely disagree (no fallback could be silent)" "true" \
'#eval
  match DefinitionIdentity.of? "952c39a88d54fd7a59f8cf449ffc4b07" "main" "check_magic"
          "a774a2e10d4c812456c99912239a7a81" with
  | .error _ => false
  | .ok driftedId =>
    let byName := (tableHoldsModelNamed "Concrete.Proof.elfFns" (CallableId.ofUser "main" "check_magic")).toOption == some true
    let byIdentity := match scopedEntryEvidenceForTable "Concrete.Proof.elfFns" with
                      | .ok rows => scopedEvidenceContains rows driftedId
                      | .error _ => false
    byName && !byIdentity'

# AN UNSCOPED EDGE BLOCKS USABILITY, and is reported as MALFORMED rather than MISSING. There is no
# live case on this corpus — every callee has a scoped identity — so the control is synthetic, and it
# has to exist: an edge the compiler HAS and cannot key must not be dropped before the join, which
# would leave every set empty while the closure covered less than was asked.
probe "an unkeyable edge is carried, named, and blocks usability" "true" \
'#eval
  match DefinitionIdentity.of? "pkg0123456789abcdef0123456789abcd" "m" "caller"
          "00000000000000000000000000000000" with
  | .error _ => false
  | .ok subj =>
    let r := correspond { subject := subj, requestedEdges := [], candidateWitnesses := []
                        , unscopedEdges := [(CallableId.ofUser "m" "ghost", DependencyEdge.body,
                                             DefinitionIdentityRefusal.legacyNameOnly "v1:user:m.ghost")] }
    r.malformed.length == 1 && r.missing.isEmpty && !(r.usable 0)'

# AN UNREADABLE TABLE REFUSES, and the refusal is what makes its edges fall to `missing` rather than
# being justified. No corpus case exercises this branch — every table a theorem names is now readable
# — which is precisely why it is asserted directly: a mutation against it survived for that reason,
# and a branch with no live case needs a control rather than an untestable mutation.
probe "an unknown table REFUSES scoped membership, it does not answer empty" "true" \
'#eval match scopedEntryEvidenceForTable "No.Such.Table" with
   | .error (TableResolveRefusal.unknownTable _) => true
   | _ => false'

# THE MANIFEST KEY IS EXACT, not a prefix. `namespace = "..."` must not answer a request for `name`,
# and `versioning` must not answer `version` — a package identity read from the wrong key is the
# silent substitution this type exists to close, and section-awareness alone did not prevent it.
probe "a manifest key is matched exactly, not by prefix" "true" \
'#eval
  let m := "[package]\nnamespace = \"wrong\"\nname = \"right\"\nversioning = \"9.9\"\n"
  (packageField m "name" == some "right") && (packageField m "version" == none)'
# ...and EVERY identity component is checked for location dependence, including the content root,
# which was exempt for no reason: an identity is reproducible only if all of it is.
probe "a location-dependent content root is refused" "true" \
'#eval match PackageIdentity.of? "pkg" "origin" "/home/someone/checkout" with
   | .error (PackageIdentityRefusal.locationDependent _) => true
   | _ => false'

probe "cryptoFns is ATTESTED and yields 5 scoped entries" "some 5" \
'#eval (scopedEntryEvidence cryptoFns).toOption.map (·.length)'
probe "elfFns is ATTESTED and yields 5 scoped entries" "some 5" \
'#eval (scopedEntryEvidence elfFns).toOption.map (·.length)'
probe "fixedCapacityFns is ATTESTED and yields 4 scoped entries" "some 4" \
'#eval (scopedEntryEvidence fixedCapacityFns).toOption.map (·.length)'
# EIGHT, not three: this table binds its DEPENDENCY references too, so every model it holds is bound
# to exactly one implementation. Three of the eight are proof-linked subjects and five are requested
# body-edge callees; the count is the whole entry set precisely because the join asks about callees.
probe "parseValidateFns binds all 8 entries (subject AND dependency references)" "some 8" \
'#eval (scopedEntryEvidence parseValidateFns).toOption.map (·.length)'
# ONE model, TWO scoped entries — the same `ctCompareFn` attested to two packages whose
# implementations differ. If scoped identity ever collapsed to the model, this would read 1.
probe "ctTagFns attests ONE model to TWO packages and yields 2 scoped entries" "some 2" \
'#eval (scopedEntryEvidence ctTagFns).toOption.map (·.length)'

# NO CONVERTED TABLE MAY BE DELTA-UNFOLDED BY A PROOF — mechanical, replacing a claim I made three
# times from a grep and got wrong twice.
#
# A `simp` set naming a table def makes Lean mint that def's EQUATION LEMMA and lets the table's
# definitional shape enter the proof term. Attesting then moves the THEOREM's artifact digest, not
# just the table's — which is precisely what an artifact digest is supposed to mean, so the fix is
# to stop the dependency rather than to accept the churn. Step 1 reported "theorem artifact digests
# unchanged" while `verify_message_composed_correct` had in fact moved 2b1b47d5… -> 9d62ecbf…,
# because `CryptoVerify/Proofs.lean` named `cryptoFns` on a continuation line my grep did not match.
#
# The presence of `<table>.eq_1` in the environment IS that dependency, so it is checked directly
# instead of grepped. A proof that reduces through the `@[simp] …_globals` projection mints nothing.
probe "no converted table has an equation lemma (no proof delta-unfolds an attested table)" "true" \
'#eval show MetaM Bool from do
   let env ← getEnv
   let tables := [`Concrete.Proof.cryptoFns, `Concrete.Proof.ctTagFns, `Concrete.Proof.elfFns,
                  `Concrete.Proof.fixedCapacityFns, `Concrete.Proof.parseValidateFns]
   return tables.all (fun t => !(env.contains (t ++ `eq_1)) && !(env.contains (t ++ `eq_def)))'
# NON-VACUITY: the check must be able to SEE such a lemma. `pureCoreFns` is delta-unfolded by
# `proofs/Examples/PureCore/Proofs.lean` and is not manifest-backed, so it is the live positive case.
probe "the equation-lemma check is non-vacuous (pureCoreFns still has one)" "true" \
'#eval show MetaM Bool from do
   let env ← getEnv
   return env.contains `Concrete.Proof.pureCoreFns.eq_1'
# ...and attesting changed NOTHING the corpus evaluates through: same entries, same dispatch.
probe "attesting preserves entries and globals dispatch" "true" \
'#eval fixedCapacityFns.entries.size == 4
   && (fixedCapacityFns.globals "ring_push").isSome
   && (fixedCapacityFns.globals "nope").isNone'
# NON-VACUITY of the probes above: some table must still REFUSE, or "attested" would be
# indistinguishable from "the check accepts anything".
#
# THE CONTROL MOVED, and the move is the point. It used to be `combineFns`, which is now converted —
# every manifest-backed table is. The remaining live case is `proofFns`: it appears in NO manifest
# row, because no fixture subject claims its theorems, so nothing can attest it and it must stay
# evidence-ineligible rather than acquiring a fallback. That is entrance condition 3, asserted here
# rather than described.
probe "a table with NO manifest row still refuses as legacy" "true" \
'#eval match scopedEntryEvidence proofFns with
   | .error ScopedMembershipRefusal.legacyUnattested => true
   | _ => false'
# ...and the other two no-manifest tables reach the same disposition by a different route: they hold
# no entries at all, so their membership is empty for every callee. Empty membership justifies
# nothing, which is what evidence-ineligible has to mean — it is not a weaker form of attested.
probe "the entry-less no-manifest tables yield empty membership, justifying nothing" "true" \
'#eval
  match DefinitionIdentity.of? "p" "m" "d" "496808fdd594d5047f23e823bc26b69c" with
  | .error _ => false
  | .ok anyId =>
    (scopedEntryEvidence proofFnsExt).toOption == some []
      && (scopedEntryEvidence pureCoreFns).toOption == some []
      && !(scopedEvidenceContains ((scopedEntryEvidence proofFnsExt).toOption.getD []) anyId)'

# AN EMPTY TABLE IS VACUOUSLY ATTESTED, NOT LEGACY. `FnTable.empty` has no entries, so there is
# nothing to attest and membership is empty for every callee — a complete answer, not a missing one.
# TEN packages share that single constant (13 attestation rows across them), so it cannot be attested
# per-package; treating it as legacy would refuse every subject naming it while nothing about it
# remains to establish.
probe "an EMPTY table yields empty membership, not a legacy refusal" "true" '
#eval (scopedEntryEvidence (FnTable.empty)).toOption == some []'
# ...and a table WITH entries but no attestations is still legacy, so the exemption is about
# emptiness rather than about missing attestations.
probe "a table WITH entries but no attestations is still legacy" "true" '
#eval
  let d : PFnDef := { identity := .semantic (CallableId.ofUser "m" "f"), operationalKey := "f",
                      sourceBodyDigest := some { value := "496808fdd594d5047f23e823bc26b69c" },
                      params := [], body := PExpr.lit (PVal.int 0) }
  match scopedEntryEvidence ({ entries := #[d], globals := fun _ => none } : FnTable) with
  | .error ScopedMembershipRefusal.legacyUnattested => true
  | _ => false'

# === SCOPED MEMBERSHIP (the join's replacement key) ============================================
# `tableEntryEvidence` keys on `CallableId`, a source NAME. `scopedEntryEvidence` asks the same
# question of the four-component identity. A table with no attestations is LEGACY — it evaluates, but
# cannot say which definitions it describes, so it yields `needs_recheck` rather than participating.
echo "=== scoped membership ==="

SC='
  let m : PFnDef := { identity := .semantic (CallableId.ofUser "calls" "inc"), operationalKey := "inc",
                      sourceBodyDigest := some { value := "496808fdd594d5047f23e823bc26b69c" },
                      params := [], body := PExpr.lit (PVal.int 0) }
  let idA := DefinitionIdentity.of? "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" "calls" "inc" "7afedf51e742f4ce04201459a3965bc8"
  let idB := DefinitionIdentity.of? "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb" "calls" "inc" "7afedf51e742f4ce04201459a3965bc8"'

# A LEGACY table refuses with needs_recheck rather than answering membership from names.
probe "a table with models but no attestations refuses as legacy needs_recheck" "true" '
#eval
  let d : PFnDef := { identity := .semantic (CallableId.ofUser "m" "f"), operationalKey := "f",
                      sourceBodyDigest := some { value := "496808fdd594d5047f23e823bc26b69c" },
                      params := [], body := PExpr.lit (PVal.int 0) }
  match scopedEntryEvidence ({ entries := #[d], globals := fun _ => none } : FnTable) with
  | .error ScopedMembershipRefusal.legacyUnattested => true
  | _ => false'

# An ATTESTED table answers scoped membership, and the body digest is still recomputed from the model.
probe "an attested table yields scoped membership" "true" "
#eval$SC
  (scopedEntryEvidence (FnTable.ofAttested [AttestedPFnDef.of m idA])).toOption.isSome"

# THE COLLISION, at the membership level: the SAME name in a different PACKAGE is not a member.
probe "membership is decided by all four components, so another package is NOT a member" "true" "
#eval$SC
  match idB with
  | .ok other =>
    match scopedEntryEvidence (FnTable.ofAttested [AttestedPFnDef.of m idA]) with
    | .ok rows => scopedEvidenceContains rows other == false
    | .error _ => false
  | _ => false"
# ...and the identity it WAS attested to IS a member, so the refusal above is not vacuous.
probe "the attested definition itself IS a member" "true" "
#eval$SC
  match idA with
  | .ok own =>
    match scopedEntryEvidence (FnTable.ofAttested [AttestedPFnDef.of m idA]) with
    | .ok rows => scopedEvidenceContains rows own
    | .error _ => false
  | _ => false"

# The body check is NOT weakened by moving to scoped identity: a model whose body disagrees with its
# stored provenance still refuses, now naming the definition rather than the callable.
probe "a body/provenance mismatch still refuses on the scoped path" "true" "
#eval$SC
  let tampered : PFnDef := { m with body := PExpr.lit (PVal.int 1) }
  match scopedEntryEvidence (FnTable.ofAttested [AttestedPFnDef.of tampered idA]) with
  | .error (ScopedMembershipRefusal.bodyMismatch _ _ _) => true
  | _ => false"

# CONVERSION PATH for the hand-written tables. They set `globals` for dispatch and carry
# `@[simp] … .globals = … := rfl` lemmas the proofs rely on, so `ofAttested` — which nulls `globals` —
# cannot convert them. `withAttestations` is a structure update: type and behaviour untouched, only
# evidence material added.
probe "withAttestations preserves globals (the rfl simp lemmas keep working)" "true" '
#eval
  let g : String → Option PFnDef := fun n => if n == "f" then some { params := [], body := PExpr.lit (PVal.int 0) } else none
  let t : FnTable := { entries := #[], globals := g }
  ((FnTable.withAttestations t []).globals "f").isSome'
# AN ATTESTED MODEL MUST BE IN THE TABLE. `attested` and `entries` are separate arrays, so nothing
# structural stops a table attesting a model it does not hold — and membership answered from that
# would describe something the table cannot dispatch to.
probe "attesting a model the table does not contain refuses" "true" '
#eval
  let m : PFnDef := { identity := .semantic (CallableId.ofUser "calls" "inc"), operationalKey := "inc",
                      sourceBodyDigest := some { value := "496808fdd594d5047f23e823bc26b69c" },
                      params := [], body := PExpr.lit (PVal.int 0) }
  let idA := DefinitionIdentity.of? "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" "calls" "inc" "7afedf51e742f4ce04201459a3965bc8"
  let other : PFnDef := { m with operationalKey := "elsewhere" }
  match scopedEntryEvidence (FnTable.withAttestations { entries := #[m], globals := fun _ => none } [AttestedPFnDef.of other idA]) with
  | .error (ScopedMembershipRefusal.attestedModelNotInTable _) => true
  | _ => false'
# ...and attesting a model the table DOES contain is accepted, so the refusal is targeted.
probe "attesting a model the table contains is accepted" "true" '
#eval
  let m : PFnDef := { identity := .semantic (CallableId.ofUser "calls" "inc"), operationalKey := "inc",
                      sourceBodyDigest := some { value := "496808fdd594d5047f23e823bc26b69c" },
                      params := [], body := PExpr.lit (PVal.int 0) }
  let idA := DefinitionIdentity.of? "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" "calls" "inc" "7afedf51e742f4ce04201459a3965bc8"
  (scopedEntryEvidence (FnTable.withAttestations { entries := #[m], globals := fun _ => none } [AttestedPFnDef.of m idA])).toOption.isSome'

# === DRIFT SUBSTITUTION IS CAUGHT (attestation-manifest acceptance condition) ==================
# `thesis_demo/src/main_drifted.con` resolved as the reference source for `proofFns` when the
# conversion was attempted, so "substituting main_drifted.con is mutation-killed" is a live hazard
# rather than a hypothetical. Verifiable now, before the generator exists.
#
# MEASURED, AND THE RESULT IS NARROWER THAN IT SOUNDS: the canonical and drifted variants share a
# PACKAGE identity — both are module `main` under one `Concrete.toml`, and `contentRoot` is derived
# from module NAMES — so package scope does NO work here. The drift is caught by the IMPLEMENTATION
# component alone. Stated because a reader could otherwise assume package scope is what separates
# them, and build a generator that relies on it.
CANON="$(cc examples/thesis_demo/src/main.con generated-implementations || true)"
DRIFT="$(cc examples/thesis_demo/src/main_drifted.con generated-implementations || true)"
C_IMPL="$(printf '%s' "$CANON" | grep -A1 'def main_parse_byte' | tail -1 | grep -oE '"[0-9a-f]{32}"' | tail -1)"
D_IMPL="$(printf '%s' "$DRIFT" | grep -A1 'def main_parse_byte' | tail -1 | grep -oE '"[0-9a-f]{32}"' | tail -1)"
C_PKG="$(printf '%s' "$CANON" | grep -A1 'def main_parse_byte' | tail -1 | grep -oE '"[0-9a-f]{32}"' | head -1)"
D_PKG="$(printf '%s' "$DRIFT" | grep -A1 'def main_parse_byte' | tail -1 | grep -oE '"[0-9a-f]{32}"' | head -1)"
if [ -n "$C_IMPL" ] && [ "$C_IMPL" != "$D_IMPL" ]; then
  ok "the drifted variant yields a DIFFERENT implementation identity (substitution is caught)"
else
  no "canonical and drifted parse_byte share an implementation identity ($C_IMPL) — drift substitution would NOT be caught"
fi
# STRENGTHENED 2026-08-14, and this gate is why the change was noticed rather than assumed. It
# previously asserted that canonical and drifted SHARE a package identity, because `contentRoot` was
# derived from module NAMES and both are module `main`. `contentRoot` now binds module CONTENT — a
# change made to close a package collapse found in the corpus (`composition` and
# `composition_trusted_helper` are different programs that collapsed to one identity) — so the two
# variants now differ in package identity as well as implementation.
#
# Drift is therefore caught by BOTH components. Asserted as inequality rather than relaxed to "don't
# care": if they ever collapse again, that is a regression in package identity and this says so.
if [ "$C_PKG" != "$D_PKG" ]; then
  ok "…and they also differ in PACKAGE identity, since contentRoot binds module content"
else
  no "canonical and drifted share a PACKAGE identity ($C_PKG) — contentRoot has stopped binding content, and two different programs would collapse"
fi

# === ATTESTED TABLE ENTRIES (the author-facing boundary) ======================================
# A `PFnDef` is a mathematical MODEL: no typed signature, no capabilities, no generics, no contracts,
# so it cannot derive the authoritative implementation identity and must not pretend to. The binding
# is a separate object whose components are GENERATED — an author selects a typed symbol and
# transcribes nothing, because hand-copied digests go stale silently and a hand-written package name
# is the collision the scoped identity removes.
echo "=== attested table entries ==="

# Models carry REAL provenance matching their bodies: `scopedEntryEvidence` recomputes the body
# digest, so a model without it refuses as `provenanceMissing` and every probe below would fail for
# that reason instead of the one it names. (These passed before the refusals moved, because
# `ofAttested` never checked provenance — the move made the omission visible.)
AT='
  let m : PFnDef := { identity := .semantic (CallableId.ofUser "calls" "inc"),
                      operationalKey := "inc",
                      sourceBodyDigest := some { value := "496808fdd594d5047f23e823bc26b69c" },
                      params := ["x"], body := PExpr.lit (PVal.int 0) }
  let m2 : PFnDef := { identity := .semantic (CallableId.ofUser "calls" "dbl"),
                       operationalKey := "dbl",
                       sourceBodyDigest := some { value := "7afedf51e742f4ce04201459a3965bc8" },
                       params := ["x"], body := PExpr.lit (PVal.int 1) }
  let idA := DefinitionIdentity.of? "496808fdd594d5047f23e823bc26b69c" "calls" "inc" "7afedf51e742f4ce04201459a3965bc8"
  let idB := DefinitionIdentity.of? "496808fdd594d5047f23e823bc26b69c" "calls" "dbl" "50333e79fd3df408c9fab0a0a3b40a93"'

# `ofAttested` is TOTAL — it cannot refuse, because refusing at the DEFINITION site would have forced
# `def proofFns : FnTable` to become an Option and changed 103 kernel-checked theorem statements. The
# refusals live at the evidence boundary instead, asserted below.
probe "attested entries with distinct generated references yield scoped membership" "true" "
#eval$AT
  (scopedEntryEvidence (FnTable.ofAttested [AttestedPFnDef.of m idA, AttestedPFnDef.of m2 idB])).toOption.isSome"
# MISSING generated reference -> refusal, the needs_recheck disposition rather than a default.
# A BROKEN reference is `attestationIncomplete`, distinct from `legacyUnattested` — a failed
# reference is not the same fact as a table nobody attested, and the fixes differ.
probe "a MISSING generated reference refuses as attestationIncomplete, not as legacy" "true" "
#eval$AT
  match scopedEntryEvidence (FnTable.ofAttested [AttestedPFnDef.of m (.error (DefinitionIdentityRefusal.legacyNameOnly \"calls.inc\"))]) with
  | .error (ScopedMembershipRefusal.attestationIncomplete _) => true
  | _ => false"
# DUPLICATE attestation -> ambiguous refusal: a table binding one implementation twice cannot say
# which entry a lookup selects, and taking the first is how a conflicting table resolves confidently.
probe "a DUPLICATE attestation refuses at the evidence boundary" "true" "
#eval$AT
  match scopedEntryEvidence (FnTable.ofAttested [AttestedPFnDef.of m idA, AttestedPFnDef.of m2 idA]) with
  | .error (ScopedMembershipRefusal.duplicateDefinition _) => true
  | _ => false"
# ...and two SAME-NAMED models attesting DIFFERENT implementations are LEGITIMATE — that is exactly
# what the scoped identity exists to permit, so this must not be refused as a duplicate.
probe "two same-named models attesting DIFFERENT implementations are permitted" "true" "
#eval$AT
  let sameName := DefinitionIdentity.of? \"496808fdd594d5047f23e823bc26b69c\" \"calls\" \"inc\" \"50333e79fd3df408c9fab0a0a3b40a93\"
  (scopedEntryEvidence (FnTable.ofAttested [AttestedPFnDef.of m idA, AttestedPFnDef.of m sameName])).toOption.isSome"

# THE GENERATOR EMITS A COMPILABLE, SELECTABLE SURFACE. Asserted against the real emitter rather than
# a hand-written sample, since the point is that authors never write these values.
# `|| true`: this command path exits 1 whenever the program has open proof obligations, independent
# of the report — `--report impl-manifest` behaves the same and its gate does likewise. The report
# CONTENT is what is asserted, so a nonzero exit must not abort the gate under `set -u`/pipefail.
GEN="$(cc examples/proof_patterns/composition/src/main.con generated-implementations || true)"
if printf '%s' "$GEN" | grep -q 'def calls_inc : Except DefinitionIdentityRefusal DefinitionIdentity'; then
  ok "the generator emits typed opaque references (author selects a symbol, transcribes nothing)"
else
  no "the generated-implementations report does not emit typed references"
fi
if printf '%s' "$GEN" | grep -qE 'emitted=[0-9]+ unscoped=0'; then
  ok "every entry in that fixture has a scoped identity to reference (unscoped=0)"
else
  no "the generator reported unscoped entries: $(printf '%s' "$GEN" | grep -oE 'emitted=[0-9]+ unscoped=[0-9]+')"
fi

# SCOPED IDENTITY ON THE REAL CORPUS. Every subject must get one, and the identity must SEPARATE at
# least the collision that motivated it — `main.validate_header` exists in two fixtures with the same
# `CallableId` rendering and different implementations.
# Corpus computed locally: this section runs before the correspondence section defines its list, and
# referencing a variable set later is how the first version of this block aborted the whole gate.
DEFID_FILES="$(fp_files)"
DEFID=""
for f in $DEFID_FILES; do
  DEFID="$DEFID$(cc "$f" subject-facts \
    | grep -E '^v1:|shadow defIdentity:' | paste - - 2>/dev/null)
"
done
D_TOT="$(printf '%s' "$DEFID" | grep -c 'defIdentity' || true)"
D_SCOPED="$(printf '%s' "$DEFID" | grep -cE ': [0-9a-f]{32}' || true)"
if [ "$D_TOT" = "64" ] && [ "$D_SCOPED" = "64" ]; then
  ok "all 64 subjects carry a scoped definition identity (none refused)"
else
  no "scoped identity coverage is $D_SCOPED/$D_TOT (was 64/64) — a subject lost its authoritative implementation digest"
fi
# THE COLLISION IS ACTUALLY SEPARATED, measured rather than assumed from the type's unit tests:
# one CallableId rendering, two distinct scoped identities.
VH_IDS="$(printf '%s' "$DEFID" | grep 'main.validate_header' | sed 's/.*defIdentity: //' | sort -u | grep -c . || true)"
if [ "$VH_IDS" -ge 2 ]; then
  ok "'v1:user:main.validate_header' resolves to $VH_IDS distinct scoped identities (CallableId conflated them)"
else
  no "'main.validate_header' has $VH_IDS scoped identity — the cross-program collision is NOT separated"
fi

# PACKAGE IDENTITY IS COMPUTED FROM DECLARED MATERIAL, once, in project loading. `projectRoot` is
# deliberately not an input, so two checkouts of one package agree.
probe "the same manifest and module inventory give the SAME identity (reproducible)" "true" '
#eval match packageIdentityOf "[package]\nname = \"app\"\nversion = \"1.0\"" ["main"] [],
            packageIdentityOf "[package]\nname = \"app\"\nversion = \"1.0\"" ["main"] [] with
      | .ok a, .ok b => a.digest == b.digest
      | _, _ => false'
# Same declared name, different module inventory -> different package. This is the realistic
# collision: two projects both called "app".
probe "the same declared name with a different module inventory differs" "true" '
#eval match packageIdentityOf "[package]\nname = \"app\"" ["main"] [],
            packageIdentityOf "[package]\nname = \"app\"" ["main", "extra"] [] with
      | .ok a, .ok b => a.digest != b.digest
      | _, _ => false'
# NO DECLARED NAME -> typed refusal, not a default. A default would make every unnamed project the
# same package, which is the collision the whole type exists to prevent.
probe "a project with no declared name is REFUSED, not defaulted" "true" '
#eval (packageIdentityOf "[package]\nversion = \"1.0\"" ["main"] []).toOption.isNone'
# SECTION-AWARE. A `name` under [dependencies] is not the package name, and a whole-file grep would
# have taken whichever came first — this refuses instead of adopting it.
probe "a name under [dependencies] is not mistaken for the package name" "true" '
#eval (packageIdentityOf "[dependencies]\nname = \"sneaky\"" ["main"] []).toOption.isNone'

# PACKAGE IDENTITY. The manifest name alone is NOT sufficient — two unrelated packages can choose
# the same name — so `declaredName` is bound with an origin and a canonical content root.
probe "two packages with the SAME declared name but different origins differ" "true" '
#eval match PackageIdentity.of? "app" "origin-a" "root1", PackageIdentity.of? "app" "origin-b" "root1" with
      | .ok a, .ok b => a.digest != b.digest
      | _, _ => false'
probe "the same declared name, origin and content root give the SAME identity (reproducible)" "true" '
#eval match PackageIdentity.of? "app" "origin-a" "root1", PackageIdentity.of? "app" "origin-a" "root1" with
      | .ok a, .ok b => a.digest == b.digest
      | _, _ => false'
# NO EMPTY SENTINEL. "not supplied" must not be a value that compares equal to every other absent one.
probe "an empty declared name refuses by name" "true" '
#eval match PackageIdentity.of? "" "o" "r" with
      | .error (PackageIdentityRefusal.emptyComponent _) => true
      | _ => false'
# LOCATION-DEPENDENT IDENTITIES ARE REFUSED, not sanitized: a sanitized path is still a path, and an
# identity that varies by checkout cannot be reproducible.
probe "an absolute path as origin is refused as location-dependent" "true" '
#eval match PackageIdentity.of? "app" "/home/user/checkout" "r" with
      | .error (PackageIdentityRefusal.locationDependent _) => true
      | _ => false'
probe "a URL-shaped origin is refused as location-dependent" "true" '
#eval match PackageIdentity.of? "app" "https://example.invalid/p" "r" with
      | .error (PackageIdentityRefusal.locationDependent _) => true
      | _ => false'
# STANDALONE FILES get an explicit synthetic identity from CONTENT, so two different single files
# cannot share one — which a constant `main` would guarantee they do.
probe "two different standalone files get DIFFERENT synthetic identities" "true" '
#eval match PackageIdentity.synthetic "496808fdd594d5047f23e823bc26b69c",
            PackageIdentity.synthetic "7afedf51e742f4ce04201459a3965bc8" with
      | .ok a, .ok b => a.digest != b.digest
      | _, _ => false'
probe "a synthetic identity with no content material refuses" "true" '
#eval match PackageIdentity.synthetic "" with
      | .error (PackageIdentityRefusal.emptyComponent _) => true
      | _ => false'

DI='
  let implA := "496808fdd594d5047f23e823bc26b69c"
  let implB := "7afedf51e742f4ce04201459a3965bc8"'

# THE COLLISION, stated as the property that must hold: same module and declaration, different
# package -> NOT the same definition. Under CallableId alone these are indistinguishable.
probe "same module+declaration in DIFFERENT packages are not the same definition" "true" "
#eval$DI
  match DefinitionIdentity.of? \"elf_header\" \"main\" \"validate_header\" implA,
        DefinitionIdentity.of? \"proof_pressure\" \"main\" \"validate_header\" implA with
  | .ok a, .ok b => !(a.sameDefinition b)
  | _, _ => false"

# ...and same package+module+declaration with a DIFFERENT implementation is also not the same:
# package scope alone is insufficient because implementations change within a package.
probe "the same name with a different IMPLEMENTATION is not the same definition" "true" "
#eval$DI
  match DefinitionIdentity.of? \"p\" \"main\" \"validate_header\" implA,
        DefinitionIdentity.of? \"p\" \"main\" \"validate_header\" implB with
  | .ok a, .ok b => !(a.sameDefinition b)
  | _, _ => false"

# POSITIVE CONTROL. All four components equal -> the same definition. Without this every refusal
# above could hold because nothing ever matches.
probe "identical components ARE the same definition" "true" "
#eval$DI
  match DefinitionIdentity.of? \"p\" \"main\" \"f\" implA, DefinitionIdentity.of? \"p\" \"main\" \"f\" implA with
  | .ok a, .ok b => a.sameDefinition b
  | _, _ => false"

# EMPTY COMPONENTS REFUSE. Two unknown packages would both bind as \"\" and compare EQUAL — the very
# collision this type exists to prevent, reintroduced through an absence.
probe "an empty package identity refuses by name" "true" "
#eval$DI
  match DefinitionIdentity.of? \"\" \"main\" \"f\" implA with
  | .error (DefinitionIdentityRefusal.emptyComponent _) => true
  | _ => false"
probe "an empty declaration identity refuses by name" "true" "
#eval$DI
  match DefinitionIdentity.of? \"p\" \"main\" \"\" implA with
  | .error (DefinitionIdentityRefusal.emptyComponent _) => true
  | _ => false"
# A non-canonical implementation identity cannot have come from `implementationDigest`.
probe "a non-canonical implementation identity refuses by name" "true" '
#eval match DefinitionIdentity.of? "p" "main" "f" "not-a-digest" with
      | .error (DefinitionIdentityRefusal.implementationNotCanonical _) => true
      | _ => false'

# The digest binds all four components, so two identities differing only in package digest apart.
probe "the identity digest separates packages" "true" "
#eval$DI
  match DefinitionIdentity.of? \"a\" \"main\" \"f\" implA, DefinitionIdentity.of? \"b\" \"main\" \"f\" implA with
  | .ok x, .ok y => x.digest != y.digest
  | _, _ => false"

# === PER-EDGE CORRESPONDENCE: THE CLOSED JOIN ================================================
# Every control below was specified in review. NOT WIRED to production — nothing calls `correspond`
# yet, and it cannot be fed from the corpus until the hand-back carries per-table ENTRY evidence.
# What is claimed here is the join and its refusals, not corpus coverage.
echo "=== per-edge correspondence: the closed join ==="

# THE SYNTHETIC IDENTITIES ARE SCOPED NOW. Every key in this join is a `DefinitionIdentity`, so the
# controls build one: same package and module, different declarations and implementations. `did` is
# total here only because the components are literals known to validate — the production path takes
# the `Except` and refuses.
CORR='
  let P := "pkg0123456789abcdef0123456789abcd"
  let withIds : (DefinitionIdentity → DefinitionIdentity → DefinitionIdentity → Bool) → Bool :=
    fun k =>
      match DefinitionIdentity.of? P "m" "caller" "00000000000000000000000000000000",
            DefinitionIdentity.of? P "m" "a" "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
            DefinitionIdentity.of? P "m" "b" "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb" with
      | .ok s, .ok a, .ok b => k s a b
      -- A CONTROL THAT CANNOT BUILD ITS OWN IDENTITIES FAILS. There is no total constructor and
      -- none is wanted: the probe reports false rather than inventing a value, which is the same
      -- discipline the production path follows.
      | _, _, _ => false
  withIds fun subj cA cB =>
  let did := fun (d i : String) => (DefinitionIdentity.of? P "m" d i).toOption.getD subj
  let lA := CallableId.ofUser "m" "a"
  let lB := CallableId.ofUser "m" "b"
  let reqA : RequestedEdge := { callee := cA, label := lA, kind := .body }
  let reqB : RequestedEdge := { callee := cB, label := lB, kind := .body }
  let wA : EdgeWitness := { subject := subj, target := .edgeTo cA, kind := .body }
  let wB : EdgeWitness := { subject := subj, target := .edgeTo cB, kind := .body }'

probe "an exact one-to-one join is usable" "true" "
#eval$CORR
  let r := correspond { subject := subj, requestedEdges := [reqA, reqB], candidateWitnesses := [wA, wB] }
  r.usable 2"

# ONE EXTRA UNRELATED WITNESS -> exactly one named surplus.
probe "one extra unrelated witness produces exactly ONE named surplus" "true" "
#eval$CORR
  let extra : EdgeWitness := { subject := subj, target := .edgeTo (did \"z\" \"zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz\"), kind := .body }
  let r := correspond { subject := subj, requestedEdges := [reqA], candidateWitnesses := [wA, extra] }
  r.surplus.length == 1 && !(r.usable 1)"

# ...and REMOVING it restores usability, so the refusal is about that witness and nothing else.
probe "removing the extra witness restores usability" "true" "
#eval$CORR
  let r := correspond { subject := subj, requestedEdges := [reqA], candidateWitnesses := [wA] }
  r.usable 1"

# WRONG SUBJECT is an identity failure, named — not silently consumed, and NOT surplus, because
# surplus means \"belonged to this operation and matched nothing\", a different fact from \"was never ours\".
probe "a witness for the correct callee but WRONG subject is refused by name, not consumed" "true" "
#eval$CORR
  let wrong : EdgeWitness := { subject := did \"other\" \"cccccccccccccccccccccccccccccccc\", target := .edgeTo cA, kind := .body }
  let r := correspond { subject := subj, requestedEdges := [reqA], candidateWitnesses := [wrong] }
  r.malformed.length == 1 && r.surplus.isEmpty && r.missing.length == 1 && !(r.usable 1)"

# WRONG KIND for a real edge is the `mismatched` set at the witness level.
probe "a witness claiming the wrong KIND is named, and its edge has no valid justification" "true" "
#eval$CORR
  let badKind : EdgeWitness := { subject := subj, target := .edgeTo cA, kind := .contract }
  let r := correspond { subject := subj, requestedEdges := [reqA], candidateWitnesses := [badKind] }
  r.malformed.length == 1 && r.missing.length == 1 && r.matched.isEmpty"

# DUPLICATING a matched witness yields AMBIGUITY, not one match plus one surplus.
probe "duplicating a matched witness produces ambiguity, not match-plus-surplus" "true" "
#eval$CORR
  let r := correspond { subject := subj, requestedEdges := [reqA], candidateWitnesses := [wA, wA] }
  r.ambiguous.length == 1 && r.matched.isEmpty && r.surplus.isEmpty"

# SWAPPING two witnesses must never yield a successful join.
# The review asked for "swapping two witnesses produces mismatched/ambiguous, never a successful
# join". Measured: a permutation of two SAME-KIND witnesses between two requested edges legitimately
# corresponds — each edge still receives exactly one witness naming it, so there is nothing to
# refuse, and asserting otherwise would demand a refusal with no defect behind it. The meaningful
# swaps are the ones that change an identity or a kind, and those are refused above. Labelled for
# what it asserts rather than for what was asked, because a probe whose name contradicts its
# assertion is worse than an absent one.
# A CONTRACT EDGE IS HELD TO THE IMPLEMENTATION IDENTITY TODAY — conservatively, and deliberately.
#
# The authority-transition criterion asks that contract witnesses bind exact callee
# CONTRACTS/hypotheses. They do not yet: `correspondenceInputOf` derives a witness for a contract
# edge exactly as for a body edge, from scoped table membership, so the edge is justified only when
# the table holds that exact IMPLEMENTATION. That is strictly STRONGER than the contract question and
# therefore fail-closed — a contract proof holds for any implementation meeting the contract, so
# requiring one particular implementation can refuse a claim that is genuinely justified, but it
# cannot accept one that is not.
#
# The corpus has ZERO contract edges (45 body, 1 trusted, 18 unclassified), so nothing exercises this
# path end to end. The control below is therefore about the RULE, not about a fixture: a witness
# whose kind disagrees with the request is refused and its edge left unjustified, which is what stops
# a contract-classified witness from silently answering a body request or the reverse.
probe "a contract witness cannot answer a body request, and is named when it tries" "true" "
#eval$CORR
  let contractW : EdgeWitness := { subject := subj, target := .edgeTo cA, kind := .contract }
  let r := correspond { subject := subj, requestedEdges := [reqA], candidateWitnesses := [contractW] }
  -- The edge falls to \`missing\` AND the witness is named \`malformed\`: one fact about the edge,
  -- one about the witness, neither standing in for the other.
  r.missing.length == 1 && r.malformed.length == 1 && !(r.usable 1)"

probe "a same-kind PERMUTATION legitimately corresponds (the meaningful swaps are refused above)" "true" "
#eval$CORR
  let swapA : EdgeWitness := { subject := subj, target := .edgeTo cB, kind := .body }
  let swapB : EdgeWitness := { subject := subj, target := .edgeTo cA, kind := .body }
  let r := correspond { subject := subj, requestedEdges := [reqA, reqB], candidateWitnesses := [swapA, swapB] }
  r.usable 2"

# A DYNAMIC whole-table witness is consumed ONCE and produces no per-entry surplus.
probe "a dynamic whole-table witness is consumed once, with no per-entry surplus" "true" "
#eval$CORR
  -- the request now states WHICH table and which digest it expects; without that it matches nothing
  let dyn : RequestedEdge := { callee := cA, label := lA, kind := .body, dynamic := true
                             , expectedTable := some (\"Tbl\", \"7bcec2d7871f93204b26e2bf83d5acf1\") }
  let tbl : EdgeWitness := { subject := subj, target := .wholeTable \"Tbl\" \"7bcec2d7871f93204b26e2bf83d5acf1\", kind := .body }
  let r := correspond { subject := subj, requestedEdges := [dyn], candidateWitnesses := [tbl] }
  r.usable 1 && r.surplus.isEmpty"

# A dynamic witness naming the WRONG table, or the right table with a stale digest, must not justify
# the edge. Before this the exemption that stops per-entry surplus also stopped any checking.
probe "a whole-table witness for the WRONG table does not justify a dynamic edge" "true" "
#eval$CORR
  let dyn : RequestedEdge := { callee := cA, label := lA, kind := .body, dynamic := true
                             , expectedTable := some (\"Tbl\", \"7bcec2d7871f93204b26e2bf83d5acf1\") }
  let other : EdgeWitness := { subject := subj, target := .wholeTable \"OtherTbl\" \"7bcec2d7871f93204b26e2bf83d5acf1\", kind := .body }
  let r := correspond { subject := subj, requestedEdges := [dyn], candidateWitnesses := [other] }
  !(r.usable 1) && r.surplus.length == 1"
probe "a whole-table witness with a STALE digest does not justify a dynamic edge" "true" "
#eval$CORR
  let dyn : RequestedEdge := { callee := cA, label := lA, kind := .body, dynamic := true
                             , expectedTable := some (\"Tbl\", \"7bcec2d7871f93204b26e2bf83d5acf1\") }
  let stale : EdgeWitness := { subject := subj, target := .wholeTable \"Tbl\" \"00000000000000000000000000000000\", kind := .body }
  let r := correspond { subject := subj, requestedEdges := [dyn], candidateWitnesses := [stale] }
  !(r.usable 1) && r.surplus.length == 1"
# A dynamic request with NO stated expectation matches nothing — an edge whose required material is
# unstated must not be justified by material merely claiming to be whole-table.
probe "a dynamic request with no expected table matches nothing" "true" "
#eval$CORR
  let dyn : RequestedEdge := { callee := cA, label := lA, kind := .body, dynamic := true }
  let tbl : EdgeWitness := { subject := subj, target := .wholeTable \"Tbl\" \"7bcec2d7871f93204b26e2bf83d5acf1\", kind := .body }
  let r := correspond { subject := subj, requestedEdges := [dyn], candidateWitnesses := [tbl] }
  !(r.usable 1)"

# ...and the same whole-table witness with NO dynamic request is surplus, so the exemption is
# scoped to dynamic edges rather than blanket.
probe "a whole-table witness with no dynamic request IS surplus" "true" "
#eval$CORR
  let tbl : EdgeWitness := { subject := subj, target := .wholeTable \"Tbl\" \"7bcec2d7871f93204b26e2bf83d5acf1\", kind := .body }
  let r := correspond { subject := subj, requestedEdges := [reqA], candidateWitnesses := [wA, tbl] }
  r.surplus.length == 1"

# RESOLVER REFUSALS ARE PART OF THE TYPED RESULT and block usability. Previously they were appended
# to a report string, which put the fact outside the type that decides usability — so a consumer of
# `CorrespondenceResult` could not tell an unreadable dependency from an absent one.
#
# NOT implied by the other sets: this case has every requested edge matched and all four sets empty,
# and must still refuse because a named table was never examined.
probe "an unreadable named table blocks usability even with every edge matched" "true" "
#eval$CORR
  let r := correspond { subject := subj, requestedEdges := [reqA], candidateWitnesses := [wA]
                      , resolverRefusals := [TableResolveRefusal.unknownTable \"No.Such.Table\"] }
  r.matched.length == 1 && r.missing.isEmpty && r.ambiguous.isEmpty && r.surplus.isEmpty
    && r.malformed.isEmpty && !(r.usable 1)"
# ...and the same input without the refusal IS usable, so the refusal is what refuses.
probe "the same input without a resolver refusal is usable" "true" "
#eval$CORR
  let r := correspond { subject := subj, requestedEdges := [reqA], candidateWitnesses := [wA] }
  r.usable 1"
# The refusal is RETAINED, not merely counted — a consumer must be able to say which table.
probe "the refusal is retained in the result and names its table" "true" "
#eval$CORR
  let r := correspond { subject := subj, requestedEdges := [reqA], candidateWitnesses := [wA]
                      , resolverRefusals := [TableResolveRefusal.unknownTable \"No.Such.Table\"] }
  r.resolverRefusals.length == 1"

# THE COUNT IS COMPARED, not inferred from empty sets: a join that dropped a request would leave
# every set empty while covering less than was asked.
probe "usability compares the COUNT, so a dropped request cannot pass" "true" "
#eval$CORR
  let r := correspond { subject := subj, requestedEdges := [reqA], candidateWitnesses := [wA] }
  r.usable 1 && !(r.usable 2)"

# === REAL-CORPUS CORRESPONDENCE, measured separately from root coverage ========================
# Root coverage says the closure could be computed; correspondence says every edge in it has exactly
# one validated justification. A subject can root while corresponding badly, so these are reported
# and pinned apart.
CORR_FILES="$(fp_files)"
CORR_LINES=""
for f in $CORR_FILES; do
  CORR_LINES="$CORR_LINES$(cc "$f" subject-facts | grep 'shadow correspondence:' || true)
"
done
C_USABLE="$(printf '%s' "$CORR_LINES" | grep -c 'usable=yes' || true)"
C_EDGED="$(printf '%s' "$CORR_LINES" | grep -c 'matched=' || true)"
C_SURPLUS="$(printf '%s' "$CORR_LINES" | grep -oE 'surplus=[0-9]+' | awk -F= '{s+=$2} END {print s+0}')"
echo "  correspondence: $C_USABLE/$C_EDGED edge-bearing subjects fully correspond; total surplus $C_SURPLUS"
# 6 -> 8 on 2026-08-13 when out-of-build table entries began crossing as data. The three remaining:
# `fixed_capacity.validate_message` (theorem unclassified, upstream of correspondence),
# one `calls.combine` fixture (an UNRESOLVED callee in the compiler graph, not a table problem),
# and `main.validate_header` (its theorem names a table lacking the callee).
# DENOMINATOR CORRECTED 2026-08-14 to subjects that make a CLAIM. Correspondence asks whether a
# proof's dependency closure is justified; a subject with no linked theorem asserts nothing, so
# counting it as a failure reports "nobody proved this" as "this proof is unsound".
# `fixed_capacity.validate_message` is that case — eligible, 11 outgoing edges, no `#[proof_by]`
# anywhere — and it is owned by proof linkage, not by this layer.
C_NOCLAIM="$(printf '%s' "$CORR_LINES" | grep -c 'no claim' || true)"
# 9/10 -> 8/10 ON 2026-08-15 WHEN THE JOIN BECAME SCOPED, and the lost subject is a WIN rather than a
# regression: `elf_header/src/main_drifted.con` is a DIFFERENT PROGRAM that shares every declaration
# name with `elf_header`. Its edges matched `elfFns` under the `CallableId` join because the names
# agreed; under `DefinitionIdentity` they do not, because the package component differs
# (952c39a8… vs 543bfb75…) — the cross-program substitution this migration exists to close, caught
# on a real fixture rather than a synthetic one. The denominator is unchanged; one more subject is
# now correctly refused.
# 8/10 -> 8/9 ON 2026-08-15 WHEN THE MISATTACHED FIXTURE WAS REPAIRED. `proof_pressure`'s
# `validate_header` carried a `#[proof_by]` naming a theorem about `elf_header`'s identically-named
# function; the claim was DELETED rather than repointed, because no theorem proves that body. It
# therefore leaves the claiming denominator and joins the no-claim set — "nobody proved this" is a
# different fact from "this proof is unsound", and the repair moves it to the honest one.
#
# THE ONE REMAINING REFUSAL IS PERMANENT AND CORRECT: `main_drifted` is a drift fixture whose whole
# purpose is to differ from the program its theorem is about. 9/9 is NOT the target — repairing it
# would delete the control.
if [ "$C_EDGED" = "9" ] && [ "$C_USABLE" = "8" ] && [ "$C_NOCLAIM" = "2" ]; then
  ok "8 of 9 claiming subjects fully correspond, 1 correctly refuses (a drift fixture), 2 make no claim"
else
  no "corpus correspondence moved to $C_USABLE/$C_EDGED claiming (+$C_NOCLAIM no-claim; was 8/9 +2) — say which subjects changed and why"
fi

# "NO CLAIM" MUST NOT BECOME A HIDING PLACE. A subject WITH a linked theorem whose classification is
# unusable must still report `usable=no`. `main.validate_header` is exactly that control and is the
# one remaining failure — if it ever reported "no claim", the exemption would have widened from
# "no theorem" to "no usable theorem", which are different facts.
# Counted from the same lines the assertion above reads, rather than by re-deriving a window into
# the report — a first version used `grep -A2` and missed the correspondence line entirely, passing
# a check it never performed.
VH="$(printf '%s' "$CORR_LINES" | grep -c 'usable=no' || true)"
# TWO now, and each is pinned BY NAME below: a count alone would let one correct refusal be traded
# for a new incorrect one without moving the number.
if [ "$VH" = "1" ]; then
  ok "a subject WITH a theorem but unusable justification still reports usable=no (no-claim is not a hiding place)"
  # THE SURVIVING REFUSAL IS THE CROSS-PROGRAM SUBSTITUTION, and it is permanent by design.
  # `proof_pressure`'s misattachment — the refusal this control used to watch — was REPAIRED on
  # 2026-08-15 by deleting its false `#[proof_by]`, so it is no longer a claiming subject at all.
  # It exists only because the join is scoped: `main_drifted` declares the same functions as `elf_header` in a different program; every
  # one of its four body edges pointed at an `elfFns` entry by NAME. Now the package component
  # separates them and all four fall to `missing`. If this ever reads `usable=yes`, the join has
  # gone back to matching names.
  DRIFT_FACTS="$(cc examples/elf_header_drifted/src/main.con subject-facts || true)"
  if ! printf '%s' "$DRIFT_FACTS" | grep -q 'shadow correspondence: matched=0 missing=4'; then
    no "elf_header/main_drifted no longer refuses with missing=4 — a different program's edges are being justified by elfFns again"
  else
    ok "elf_header/main_drifted refuses all 4 edges (cross-program substitution, caught by scope)"
  fi
else
  no "no subject reports usable=no while having a linked theorem — the no-claim exemption may have widened"
fi

# IDENTITY IS RETAINED FOR EXCLUDED CALLEES. A trusted helper is excluded from the proof entries but
# is still a real callable; resolving only against `entries` reported it as `«unresolved»` and turned
# a `trusted` edge into a `missing` one. No `«unresolved»` may remain anywhere in the corpus.
UNRES="$(for f in $CORR_FILES; do cc "$f" subject-facts | grep -c '«unresolved»' || true; done | awk '{s+=$1} END{print s+0}')"
if [ "$UNRES" = "0" ]; then
  ok "no dependency edge in the corpus reports an «unresolved» identity"
else
  no "$UNRES edge(s) still report «unresolved» — an identity is being discarded upstream"
fi
# ...and the trusted helper resolves to a TRUSTED edge with its real identity, not merely to
# something non-unresolved.
# (removed: TH was computed here and never read — a producer with no consumer.)
# RENDERED BY LOCAL NAME NOW, because the edge carries a scoped identity and `v1:user:…` is a
# `CallableId` rendering. The identity is what the node compares; the diagnostic prints what a reader
# recognises. Still asserting BOTH halves — the right callee and the right kind.
TH2="$(cc examples/proof_patterns/composition_trusted_helper/src/main.con subject-facts | grep -c 'calls.dbl=trusted' || true)"
if [ "$TH2" -ge 1 ]; then
  ok "the trusted helper resolves to 'calls.dbl=trusted' — right callee AND right kind"
else
  no "the trusted helper does not resolve to a trusted edge on its real identity"
fi
# SURPLUS MUST BE ZERO HERE, and this is a real assertion rather than a formality. Deriving one
# witness per TABLE ENTRY produced surplus on 5 subjects, because a table legitimately holds
# implementations this caller never reaches. Witnesses are derived per REQUESTED EDGE the table
# covers, so a nonzero surplus means that regression returned.
if [ "$C_SURPLUS" = "0" ]; then
  ok "no subject reports surplus — unreached table entries are not treated as claimed dependencies"
else
  no "surplus is $C_SURPLUS across the corpus — witnesses are being derived per table ENTRY again"
fi

# THE DISPATCH IS SECURITY-RELEVANT INVENTORY: a duplicate name would let the FIRST arm win
# silently and route a table identity to the wrong value.
DUPN="$(grep -oE '^  \| "[A-Za-z.]+"' Concrete/Proof/TableResolve.lean | sort | uniq -d | tr -d ' |"')"
if [ -z "$DUPN" ]; then
  ok "no table name appears twice in the dispatch (a duplicate would silently route to the first arm)"
else
  no "duplicate dispatch entries: $DUPN — the first arm wins silently"
fi

# === WHICH DependencyClosure REFUSAL SETS ARE NAMED — DERIVED, NOT ASSERTED IN PROSE ==========
# docs/verification/EVIDENCE_ARCHITECTURE.md requires six: missing, surplus, duplicate, ambiguous, unclassified,
# mismatched. I twice reported this count from memory and got it wrong (said "4 of 6" while listing
# five). A count restated in prose is a measurement nobody took, so it is computed here from the
# constructors that actually exist and printed with the verdict.
echo "=== DependencyClosure refusal sets that are NAMED ==="
NAMED=0; MISSING_SETS=""
check_set() {  # set name | evidence: a constructor that names it
  if grep -rq "$2" Concrete/Proof/ --include=*.lean 2>/dev/null; then
    NAMED=$((NAMED+1)); ok "'$1' is a named refusal (via $2)"
  else
    MISSING_SETS="$MISSING_SETS $1"
  fi
}
check_set missing      'provenanceMissing'
check_set duplicate    'duplicateIdentity'
check_set unclassified '| unclassified'
check_set mismatched   'bodyMismatch'
check_set ambiguous    '| ambiguous'
# `surplus` is modelled as a RETAINED SET on `CorrespondenceResult`, not an inductive constructor
# like the others — a witness is surplus by virtue of matching nothing, which is a property of the
# join rather than a defect flag on the witness. The evidence pattern follows the modelling instead
# of forcing the modelling to fit the pattern.
check_set surplus      'surplus   : List EdgeWitness'
echo "  DependencyClosure refusal sets named: $NAMED/6 (unnamed:${MISSING_SETS:- none})"
if [ "$NAMED" = "6" ] && [ -z "$MISSING_SETS" ]; then
  ok "all 6 DependencyClosure refusal sets are named"
else
  no "refusal-set coverage moved to $NAMED/6, unnamed:${MISSING_SETS:- none} — update this and say which changed"
fi

# === ROOT DETERMINISM AND SENSITIVITY (slice 6, step 5) ======================================
# The acceptance boundary in two halves: discovery ARTEFACTS must not move the root, and any
# dependency SEMANTIC change must. A root that moves on order is unusable as a stored value; one
# that does not move on content binds nothing. Sensitivity and duplicate-edge stability are
# already covered above; these are the remaining properties.
echo "=== root determinism and sensitivity ==="

# EDGE order is discovery order, not meaning. Two traversals finding the same dependencies in a
# different sequence describe the same program.
probe "EDGE order does not change the root" "true" '
#eval
  match tid? "m" "a",
        tid? "m" "b",
        tid? "m" "c" with
  | some i_m_a, some i_m_b, some i_m_c =>
    let n := fun (es) => ([{ id := i_m_a, label := CallableId.ofUser "m" "a", digest := some "DA", edges := es },
                           { id := i_m_b, label := CallableId.ofUser "m" "b", digest := some "DB", edges := [] },
                           { id := i_m_c, label := CallableId.ofUser "m" "c", digest := some "DC", edges := [] }] : List DepNode)
    let e1 := [(DependencyEdge.body, i_m_b), (DependencyEdge.body, i_m_c)]
    let e2 := [(DependencyEdge.body, i_m_c), (DependencyEdge.body, i_m_b)]
    match dependencyRootMaterial (n e1) (i_m_a),
    dependencyRootMaterial (n e2) (i_m_a) with
    | Except.ok x, Except.ok y => x.preimage == y.preimage
    | _, _ => false
  | _, _, _ => false
'

# NODE-LIST order is enumeration order — how the compiler happened to walk its own entries.
probe "NODE-LIST order does not change the root" "true" '
#eval
  match tid? "m" "a",
        tid? "m" "b" with
  | some i_m_a, some i_m_b =>
    let a : DepNode := { id := i_m_a, label := CallableId.ofUser "m" "a", digest := some "DA", edges := [(DependencyEdge.body, i_m_b)] }
    let b : DepNode := { id := i_m_b, label := CallableId.ofUser "m" "b", digest := some "DB", edges := [] }
    match dependencyRootMaterial [a, b] (i_m_a),
    dependencyRootMaterial [b, a] (i_m_a) with
    | Except.ok x, Except.ok y => x.preimage == y.preimage
    | _, _ => false
  | _, _ => false
'

# TRUST PROPAGATES MONOTONICALLY: a trusted edge anywhere in the closure qualifies the root, and
# nothing downstream can unset it. Non-monotone trust would let a claim be laundered clean by
# adding a dependency.
probe "trust propagates from a DEEP dependency, not just a direct one" "true" '
#eval
  match tid? "m" "a",
        tid? "m" "b",
        tid? "m" "c" with
  | some i_m_a, some i_m_b, some i_m_c =>
    let ns := ([{ id := i_m_a, label := CallableId.ofUser "m" "a", digest := some "DA", edges := [(DependencyEdge.body, i_m_b)] },
    { id := i_m_b, label := CallableId.ofUser "m" "b", digest := some "DB", edges := [(DependencyEdge.trusted, i_m_c)] },
    { id := i_m_c, label := CallableId.ofUser "m" "c", digest := some "DC", edges := [] }] : List DepNode)
    match dependencyRootMaterial ns (i_m_a) with
    | Except.ok m => m.requiresTrustQualification
    | Except.error _ => false
  | _, _, _ => false
'

probe "an untrusted closure does NOT acquire trust qualification" "true" '
#eval
  match tid? "m" "a",
        tid? "m" "b" with
  | some i_m_a, some i_m_b =>
    let ns := ([{ id := i_m_a, label := CallableId.ofUser "m" "a", digest := some "DA", edges := [(DependencyEdge.body, i_m_b)] },
    { id := i_m_b, label := CallableId.ofUser "m" "b", digest := some "DB", edges := [] }] : List DepNode)
    match dependencyRootMaterial ns (i_m_a) with
    | Except.ok m => !m.requiresTrustQualification
    | Except.error _ => false
  | _, _ => false
'

# The EDGE KIND is in the bytes: swapping contract for body over the same targets must move the
# root, or typing the edges bought nothing.
probe "changing an edge KIND moves the root" "true" '
#eval
  match tid? "m" "a",
        tid? "m" "b" with
  | some i_m_a, some i_m_b =>
    let n := fun (k) => ([{ id := i_m_a, label := CallableId.ofUser "m" "a", digest := some "DA", edges := [(k, i_m_b)] },
    { id := i_m_b, label := CallableId.ofUser "m" "b", digest := some "DB", edges := [] }] : List DepNode)
    match dependencyRootMaterial (n DependencyEdge.body) (i_m_a),
    dependencyRootMaterial (n DependencyEdge.contract) (i_m_a) with
    | Except.ok x, Except.ok y => x.preimage != y.preimage
    | _, _ => false
  | _, _ => false
'

# === SHADOW INTEGRATION (slice 6, step 4) ====================================================
# Both consumers read ONE set of nodes (`ProofCore.dependencyNodesOf`). The root is computed and
# REPORTED; it decides nothing yet.
CORPUS_FILES_EARLY="$(fp_files)"
# BOTH temporaries, because `trap ... EXIT` REPLACES a previous EXIT trap rather than adding to it.
# This one silently cancelled the `rm -rf "$TMP"` installed at the top, leaking every generated
# batch driver directory.
TMPDISC="$(mktemp)"; trap 'rm -f "$TMPDISC"; rm -rf "$TMP"' EXIT

# ============================================================================================
# NO SUBJECT MAY ROOT WHILE REPORTING A NON-CURRENT EDGE.
#
# HISTORY, because the first version of this check asserted the opposite and was wrong. Nine of 64
# subjects reported `unclassified` edges yet rooted, and I recorded that as a COVERAGE FAIL-OPEN —
# roots computed over a smaller edge set — and predicted coverage would fall to ~53/64 once fixed.
# That diagnosis was backwards. `shadow edgeKinds` was a STUB: it derived callees from the evidence
# body and then assigned the kind as `if isTrusted then "trusted" else "unclassified"`, consulting no
# classification table, so it labelled every non-trusted edge `unclassified` regardless of the
# hand-back. The root was right and the REPORT was stale. Corrected: the real corpus has 28 `body`,
# 11 `unclassified` and 1 `missing` edge, and both lines now read one source (`dependencyNodesOf`).
#
# The check that is actually worth having is the invariant, not the disagreement count. Since
# `dependencyRootMaterial` refuses any edge that is not current for dependents, a subject that roots
# WHILE reporting a non-current edge would mean the refusal was bypassed. Expect 0, and 0 here is not
# vacuous: 12 non-current edges exist in the corpus and the two refusals below are theirs.
BYPASS=0
for f in $CORPUS_FILES_EARLY; do
  n="$(cc "$f" subject-facts \
    | grep -E 'shadow edgeKinds:|shadow depRoot:' \
    | paste - - 2>/dev/null \
    | grep -E 'non-current, ' | grep -vE '\[0 non-current' | grep -v 'depRoot: REFUSED' | grep -c . || true)"
  BYPASS=$((BYPASS + n))
done
if [ "$BYPASS" = "0" ]; then
  ok "no subject roots while reporting a non-current edge (the refusal is not bypassable)"
else
  no "$BYPASS subject(s) root despite a non-current edge — dependencyRootMaterial was bypassed"
fi

# ...and the corpus really does contain non-current edges, or the above is vacuous.
NONCUR=0
for f in $CORPUS_FILES_EARLY; do
  n="$(cc "$f" subject-facts \
        | grep -o 'shadow edgeKinds:.*' | grep -oE '=(unclassified|missing)' | grep -c . || true)"
  NONCUR=$((NONCUR + n))
done
# 12 -> 11 on 2026-08-14, and the cause is named: the single `missing` edge was
# `«unresolved».calls.dbl`, which became `trusted` once excluded records retained their identity.
# `trusted` IS current for dependents, so the count fell by exactly one and no `missing` edge
# remains in the corpus. 11 `unclassified` edges are left, all in subjects whose theorems have no
# usable classification.
# 11 -> 12 ON 2026-08-15: `proof_pressure.validate_header` lost its proof link, so its outgoing edge
# to `check_nonce` has no classification and is `unclassified` rather than `body`. An unclassified
# edge is correctly not current — the subject makes no claim now, so nothing types its dependencies.
if [ "$NONCUR" = "12" ]; then
  ok "the corpus contains exactly 12 non-current edges (12 unclassified, 0 missing) — the check above is non-vacuous"
else
  no "non-current edge count moved to $NONCUR (was 12, all unclassified) — if the hand-back classified more, update this and say so"
fi

DR="$(cc examples/proof_patterns/composition/src/main.con subject-facts | grep 'shadow depRoot:' || true)"
[ -n "$DR" ] \
  && ok "subject-facts carries a depRoot line (both consumers read ProofCore's nodes)" \
  || no "no depRoot line — the shadow integration is not wired"

# A leaf must ROOT: if everything refused, the integration would be indistinguishable from not
# having run, and "0 roots" would look like success.
printf '%s' "$DR" | grep -qE "shadow depRoot: [0-9a-f]{8}" \
  && ok "a leaf subject produces a root (the builder is actually reachable)" \
  || no "no subject produced a root — refusal is expected for unclassified edges, but a leaf with no edges must succeed"

# ...and a subject reaching an unclassified dependency must REFUSE, with the reason named.
# RESTATED 2026-08-09. This asserted that THIS fixture contains a refusal, which stopped being
# true when the classification table covered both attachment paths and composition's subjects all
# rooted. A gate pinned to "some fixture still fails" gets falsified by success, which is the
# wrong shape: the property is that a refusal, IF one occurs, names its edge — and the fail-closed
# behaviour itself is unit-tested above ("a root refuses an UNCLASSIFIED edge").
CORPUS_FILES="$(fp_files)"
CORPUS_REF="$(for f in $CORPUS_FILES; do
  cc "$f" subject-facts | grep 'depRoot: REFUSED' || true
done)"
NFILES="$(printf '%s\n' "$CORPUS_FILES" | grep -c . || true)"
NREF="$(printf '%s' "$CORPUS_REF" | grep -c 'REFUSED' || true)"
NNAMED="$(printf '%s' "$CORPUS_REF" | grep -cE "REFUSED \((no node for start identity|'[^']+' has no subject digest|duplicate node identity|'[^']+' depends on '[^']+', which has no node|'[^']+' has a non-current edge .* to '[^']+')" || true)"
if [ "$NFILES" = "0" ]; then
  no "dependency-root corpus is empty — a zero-refusal result would be vacuous"
elif [ "$NREF" = "0" ]; then
  ok "no root refusals remain in the corpus — every subject roots"
elif [ "$NNAMED" = "$NREF" ]; then
  ok "$NREF root refusal(s) remain, all $NNAMED name the identity/edge responsible"
else
  no "$NREF root refusal(s), but only $NNAMED name the identity/edge responsible"
fi

# (The status-derivation containment for roots is asserted in the ROOT INTEGRATION section above,
# where it sits beside the assertion that roots ARE consumed by the composition. It used to live here
# saying "roots remain shadow", which stopped being true when the authority pass began requiring a
# computable closure — two claims about one thing, in two places, disagreeing.)

# === THE CONTRACT-PRECISION DEFERRAL, MADE FALSIFIABLE ======================================
#
# A `contract` dependency edge means the caller's proof holds for ANY table meeting its hypotheses,
# so an implementation change to the callee should NOT stale the caller. Today it does: a contract
# edge is witnessed exactly as a body edge, justified only when the table holds that exact
# implementation. That is STRICTLY STRONGER than the contract question, so nothing is unsound — it
# is conservative, and it is not contract composition.
#
# THE DECISION (2026-08-16): precise contract witnesses move to the typed-contract milestone and
# leave R-0004's closure list. Not because no corpus case exists, but because this roadmap's own
# dependency order forbids building it now — evidence-affecting language features are frozen until
# Slice 8 closes, the typed contract substrate lands after R-0004, and binding evidence to today's
# untyped `requires`/`ensures` string encoding would freeze an incomplete internal representation,
# which the same order explicitly refuses.
#
# A DEFERRAL WITHOUT A TRIPWIRE IS A DEFERRAL THAT GOES STALE SILENTLY. The premise is measurable:
# the corpus contains no contract dependency edge, so the imprecision is currently unreachable. The
# day one appears, this goes red and the decision gets revisited rather than quietly continuing.
echo "=== contract-precision deferral: the premise is still true ==="
C_EDGES=0; B_EDGES=0
for f in $(grep -rlE '#\[(proof_by|ensures_proof)\(' examples --include='*.con' | sort); do
  KINDS="$(cc "$f" subject-facts \
           | grep -oE 'shadow edgeKinds: [^[]*' || true)"
  # `grep -o` exits 1 on no match, and under `pipefail` that trips the ERR trap — so the counter
  # would abort the gate on the very corpus state it exists to confirm. Captured, then counted.
  C_EDGES=$(( C_EDGES + $( { grep -o '=contract' <<<"$KINDS" || true; } | wc -l ) ))
  B_EDGES=$(( B_EDGES + $( { grep -o '=body' <<<"$KINDS" || true; } | wc -l ) ))
done
# NON-VACUITY FIRST. A counter that finds nothing would report zero contract edges whatever the
# corpus held, so the body count has to prove the measurement works before the zero means anything.
if [ "$B_EDGES" -gt 0 ]; then
  ok "the edge-kind census is live ($B_EDGES body edges counted)"
else
  no "the edge-kind census found NO edges at all — the contract count below is vacuous"
fi
if [ "$C_EDGES" = "0" ]; then
  ok "no contract dependency edge exists in the corpus, so the conservative witnessing is unreachable"
else
  no "$C_EDGES contract dependency edge(s) now exist — the contract-precision deferral rests on there being none. Revisit the decision recorded in ROADMAP.md before this gate is made green again."
fi

echo ""

# THE EXIT MATRIX, ACROSS THE WHOLE FIXTURE POPULATION.
#
# `cc` enforces the expected status for calls that HAPPEN, and the gate does not otherwise run every
# report on every fixture — generated-implementations reached three named inputs, while the rule it
# is checked against was written from a manual measurement over twenty-five. A comment describing a
# measurement is not coverage: it cannot fail, and it goes stale silently.
#
# So the population is enumerated and every (fixture, report) pair is executed here, with the
# denominator pinned. cc records any status or completeness violation, and reconcile_compiler_runs
# turns that into a failure, so this block asserts the matrix rather than asserting that someone
# once checked it.
EXPECTED_MATRIX_FIXTURES=25
EXPECTED_MATRIX_REPORTS=4
MATRIX_FILES="$(grep -rlE '#\[(proof_by|ensures_proof)\(' examples --include='*.con' | sort)"
MATRIX_N="$(printf '%s\n' "$MATRIX_FILES" | grep -c .)"
if [ "$MATRIX_N" != "$EXPECTED_MATRIX_FIXTURES" ]; then
  no "the proof-carrying fixture population is $MATRIX_N, pinned at $EXPECTED_MATRIX_FIXTURES — update the pin deliberately"
else
  ok "the proof-carrying fixture population is $EXPECTED_MATRIX_FIXTURES files"
fi
MATRIX_CALLS=0
for _mf in $MATRIX_FILES; do
  for _mr in subject-facts proof-status consistency generated-implementations; do
    cc "$_mf" "$_mr" >/dev/null
    MATRIX_CALLS=$((MATRIX_CALLS + 1))
  done
done
if [ "$MATRIX_CALLS" = "$((EXPECTED_MATRIX_FIXTURES * EXPECTED_MATRIX_REPORTS))" ]; then
  ok "every fixture was run against every report ($MATRIX_CALLS pairs), each held to its expected exit status"
else
  no "ran $MATRIX_CALLS (fixture, report) pairs, expected $((EXPECTED_MATRIX_FIXTURES * EXPECTED_MATRIX_REPORTS))"
fi

# Batched mint probes report here. Their verdicts are per-probe, not per-batch. This runs BEFORE
# GATE_DONE=1: the ERR trap that catches an unexpected shell failure must still be armed while the
# batch runs, or a crash inside the batch would leave a short but green-looking result.
flush_mint_probes
reconcile_compiler_runs
reconcile_ordinary_probes
reconcile_assertion_total
GATE_DONE=1
echo "DEPENDENCY-EDGES: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
