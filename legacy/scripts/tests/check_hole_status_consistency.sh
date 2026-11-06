#!/usr/bin/env bash
# Hole-status consistency gate.
#
# WHY THIS EXISTS. On 2026-07-31 three files said "H23 closed as a compile-time fact"  HOLE-STATUS-OK: quoting the wrong claim
# while docs/verification/KNOWN_HOLES.md said "H23 — OPEN, reproduced" and
# check_known_wrong_corpus.sh ASSERTED it still reproduces. Every gate was green. The
# contradiction survived because the claims live in different files and nothing compared
# them: check_docs_drift.sh verifies that referenced artifacts EXIST, not that statements
# agree, and a passing corpus gate says nothing about what the prose next to it claims.
#
# That is the same cross-document blindness recorded in 5a0c4e3e, and it had already been
# named as a class before it recurred. This gate closes it for the one kind of statement
# where being wrong is worst: whether a known soundness hole is open.
#
# THREE CHECKS, strongest first:
#   1. behaviour vs status — a hole with a live counterexample fixture must be OPEN
#   2. authority          — every hole heading carries exactly one parseable status
#   3. prose              — no other file asserts a status contradicting KNOWN_HOLES
#
# Check 1 is the one with teeth: it ties documented status to OBSERVED behaviour rather
# than to other prose. Check 3 is heuristic by nature (English), so it has an explicit
# escape hatch — see HOLE-STATUS-OK below — rather than pretending to be exact.

set -uo pipefail
source "$(dirname "${BASH_SOURCE[0]}")/lib/gate_env.sh" || exit 2
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
KH="docs/verification/KNOWN_HOLES.md"
CORPUS="scripts/tests/check_known_wrong_corpus.sh"
PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }

[ -f "$KH" ] || { echo "FAIL $KH missing"; exit 1; }

# --- parse the authority -----------------------------------------------------------
# KNOWN_HOLES.md is the single source of truth for hole status. Everything else is a
# reference to it, and a reference that disagrees is a bug in the reference.
OPEN_HOLES=""; CLOSED_HOLES=""
while IFS= read -r line; do
  id="$(printf '%s' "$line" | grep -oE '^### (H[0-9]+)' | awk '{print $2}')"
  [ -n "$id" ] || continue
  if has_match "OPEN" <<<"$line"; then OPEN_HOLES="$OPEN_HOLES $id"
  elif has_match "CLOSED" <<<"$line"; then CLOSED_HOLES="$CLOSED_HOLES $id"
  else no "hole heading has no parseable status: $line"; fi
done < <(grep -E '^### H[0-9]+\.' "$KH")

echo "=== 2. every hole heading carries exactly one status ==="
# Ambiguity here would make checks 1 and 3 silently vacuous, so it is asserted before
# they run rather than assumed.
DUP=0
for h in $OPEN_HOLES; do
  case " $CLOSED_HOLES " in *" $h "*) no "$h is marked BOTH open and closed"; DUP=1;; esac
done
[ "$DUP" -eq 0 ] && ok "no hole is marked both open and closed"
[ -n "$OPEN_HOLES" ] && ok "open holes parsed:$OPEN_HOLES" || no "no open holes parsed — regex drift?"
[ -n "$CLOSED_HOLES" ] && ok "closed holes parsed:$CLOSED_HOLES" || no "no closed holes parsed — regex drift?"

echo "=== 1. behaviour vs status: a hole with a live fixture must be OPEN ==="
# The strong check. check_known_wrong_corpus.sh asserts these fixtures still REPRODUCE
# their hole; if one is reproducing, the hole is by definition not closed, whatever any
# document says. This is the check that would have caught the H23 contradiction.
if [ -f "$CORPUS" ]; then
  # A corpus block is one of two kinds, and its heading must say which:
  #   `=== Hnn: ...`                  the fixture REPRODUCES the hole      -> must be OPEN
  #   `=== Hnn (CLOSED <date>): ...`  the fixture GUARDS the fix           -> must be CLOSED
  # Both are legitimate — a closed hole keeps its fixture precisely so a regression is red.
  #
  # Every hole id mentioned in a corpus heading must match one of the two forms. The
  # earlier version parsed only the first, so renaming H23's block to `=== H23 (CLOSED ...)`
  # silently removed it from this check: the gate went green because it stopped looking, not
  # because it verified anything. Unclassifiable headings are therefore a FAILURE, not a skip.
  HEADINGS="$(grep -oE '^echo "=== H[0-9]+[^"]*' "$CORPUS")"
  ALL_IDS="$(printf '%s' "$HEADINGS" | grep -oE 'H[0-9]+' | sort -u)"
  # `tr` to SPACES, not newlines. The `case " $REPRO " in *" $h "*` tests below match on
  # space-delimited words; `sort -u` emits newline-delimited ones. With a single id per
  # category the two forms coincide and this was invisible — it broke the moment H24 joined
  # H23 as a guarded hole, reporting BOTH as unclassifiable. A check whose correctness
  # depends on how many items it is checking is one that passes for the wrong reason.
  REPRO="$(printf '%s\n' "$HEADINGS" | grep -E '^echo "=== H[0-9]+:' | grep -oE 'H[0-9]+' \
           | sort -u | tr '\n' ' ')"
  GUARD="$(printf '%s\n' "$HEADINGS" | grep -E '^echo "=== H[0-9]+ \(CLOSED [0-9]{4}-[0-9]{2}-[0-9]{2}\):' \
           | grep -oE 'H[0-9]+' | sort -u | tr '\n' ' ')"
  if [ -z "$ALL_IDS" ]; then
    no "could not parse hole ids from $CORPUS — the link between behaviour and status is broken"
  else
    for h in $ALL_IDS; do
      IS_REPRO=0; IS_GUARD=0
      case " $REPRO " in *" $h "*) IS_REPRO=1 ;; esac
      case " $GUARD " in *" $h "*) IS_GUARD=1 ;; esac
      if [ "$IS_REPRO" = 1 ] && [ "$IS_GUARD" = 1 ]; then
        no "$h has BOTH a reproducing and a regression-guard block in $CORPUS — which is it?"
      elif [ "$IS_REPRO" = 1 ]; then
        case " $OPEN_HOLES " in
          *" $h "*) ok "$h has a reproducing fixture and is marked OPEN" ;;
          *) no "$h has a REPRODUCING fixture in $CORPUS but is not marked OPEN in $KH" ;;
        esac
      elif [ "$IS_GUARD" = 1 ]; then
        case " $CLOSED_HOLES " in
          *" $h "*) ok "$h has a regression-guard fixture and is marked CLOSED" ;;
          *) no "$h's fixture claims the hole is CLOSED but $KH does not mark it closed" ;;
        esac
      else
        no "$h appears in a $CORPUS heading that matches neither form — coverage is silently lost;\
 use '=== $h: ...' (reproduces) or '=== $h (CLOSED YYYY-MM-DD): ...' (guards the fix)"
      fi
    done
  fi
else
  no "$CORPUS missing — cannot tie hole status to observed behaviour"
fi

echo "=== 3. no other file contradicts the authority ==="
# Heuristic and deliberately so: this greps English. A line may opt out with the marker
# HOLE-STATUS-OK, which exists for the legitimate case of QUOTING a wrong claim in order
# to correct it — the correction of the H23 overclaim does exactly that, and a gate that
# forbade it would punish the fix rather than the defect.
SCAN="$(git ls-files 'docs/*.md' 'research/*.md' 'research/**/*.md' '*.md' 'scripts/tests/*.sh' 'Concrete/**/*.lean' 'Main.lean' 2>/dev/null | grep -v "^$KH$")"
VIOL=0
for h in $OPEN_HOLES; do
  # Only INDICATIVE claims count. A roadmap task legitimately says "closes H23" as its
  # objective, a gate legitimately says "H24 may be FIXED — update the docs" as a future
  # condition, and a merge bar legitimately says "H23 must be closed first". None of
  # those assert the hole IS closed, so modal and conditional phrasing is filtered out
  # rather than marked one line at a time. What remains is the assertive form, which is
  # the form that misleads.
  # NEGATION IS NOT ASSERTION, and leaving it out made this gate report the opposite of the truth.
  # `VERIFICATION_STATUS.md` said "H25 remains contained, not fixed" — a sentence stating H25 is
  # OPEN — and the heuristic read `H25 ... fixed`, matched, and accused the file of claiming closure.
  # Marking that line HOLE-STATUS-OK was the available shortcut and the wrong one: it would except a
  # CORRECT sentence and leave the heuristic wrong for the next person who writes an accurate one,
  # turning an opt-out meant for quoting a wrong claim into a place to hide the gate's own defect.
  HITS="$(grep -rniE "\b$h\b[^.]{0,40}(is )?(now )?(closed|fixed|resolved)" $SCAN 2>/dev/null \
          | grep -viE "(must|may|should|will|would|could|once|until|pending|before|when|if) (be |it |the )?(closed|fixed|resolved)" \
          | grep -viE "must be closed|may be fixed|to be (closed|fixed)" \
          | grep -viE "(not|never|isn't|aren't|un)[ -]*(yet )?(closed|fixed|resolved)" \
          | grep -v "HOLE-STATUS-OK" || true)"
  if [ -n "$HITS" ]; then
    VIOL=1
    no "$h is OPEN in $KH but claimed closed elsewhere:"
    printf '%s\n' "$HITS" | head -4 | sed 's/^/         /'
  fi
done
[ "$VIOL" -eq 0 ] && ok "no file claims an OPEN hole is closed"

# NON-VACUITY FOR THE FILTER ITSELF. Every exclusion above makes this leg blinder, and the negation
# exclusion was added because the gate misfired — precisely the circumstance in which it is tempting
# to widen a filter until the complaint stops. So the filter is run against synthetic text with a
# known answer: an assertive claim MUST still be caught, and a negated one MUST NOT be. Without this,
# a filter that had swallowed everything would report "no file claims an OPEN hole is closed" and
# look identical to a clean tree.
FILTER_PROBE="$(mktemp -d)"; trap 'rm -rf "$FILTER_PROBE"' EXIT
h_probe="$(printf '%s' "$OPEN_HOLES" | tr ' ' '\n' | grep -v '^$' | head -1)"
if [ -n "$h_probe" ]; then
  printf '%s is now closed and needs no further work.\n' "$h_probe" > "$FILTER_PROBE/assertive.md"
  printf '%s remains contained, not fixed.\n' "$h_probe" > "$FILTER_PROBE/negated.md"
  probe_filter() {
    grep -rniE "\b$h_probe\b[^.]{0,40}(is )?(now )?(closed|fixed|resolved)" "$1" 2>/dev/null \
      | grep -viE "(must|may|should|will|would|could|once|until|pending|before|when|if) (be |it |the )?(closed|fixed|resolved)" \
      | grep -viE "must be closed|may be fixed|to be (closed|fixed)" \
      | grep -viE "(not|never|isn't|aren't|un)[ -]*(yet )?(closed|fixed|resolved)" \
      | grep -v "HOLE-STATUS-OK" || true
  }
  if [ -n "$(probe_filter "$FILTER_PROBE/assertive.md")" ]; then
    ok "CONTROL: the filter still catches an assertive '$h_probe is now closed'"
  else
    no "CONTROL FAILED: the filter no longer catches an assertive closure claim — check 3 is inert and its pass above proves nothing"
  fi
  if [ -z "$(probe_filter "$FILTER_PROBE/negated.md")" ]; then
    ok "CONTROL: the filter correctly ignores a negated '$h_probe ... not fixed'"
  else
    no "CONTROL FAILED: the filter still flags a negated claim — an accurate sentence reads as a violation"
  fi
else
  no "no OPEN holes parsed from $KH — check 3 and its controls are vacuous"
fi

# The OTHER direction, which this gate lacked until 2026-08-03. Closing H23 left
# `Report.lean` still saying "Not H23 itself, which stays OPEN: nothing populates an   HOLE-STATUS-OK: quoting the wrong claim to correct it
# assumption set until R-0461" — an assertion that had become false, sitting in the exact
# module the fix landed in. Check 3 above only looked for open-claimed-closed, so it passed.
#
# This direction matters MORE than it sounds, because its failure mode is the inverse of the
# one this gate was built for: prose that understates what the compiler now guarantees
# teaches a reader to distrust a check that works, and the next person to touch that code
# reads the comment, not KNOWN_HOLES. Symmetry here is not tidiness — a status is only
# trustworthy if BOTH kinds of drift are loud.
#
# The exclusion list here is deliberately NARROWER than check 3's above: only genuine
# past-tense markers. `until`/`before` are excluded up there because "H24 is open until  HOLE-STATUS-OK: quoting the phrasing pattern, not asserting a status
# R-0464" is a legitimate future condition; down here "H23 stays OPEN ... until R-0461" is  HOLE-STATUS-OK: quoting the stale form to explain it
# the STALE form itself, and filtering on `until` is what hid Report.lean:2427 on this
# check's first run. Same word, opposite meaning, depending on the hole's status.
# BOTH WORD ORDERS. The first pattern catches "H23 is still open"; the second catches   HOLE-STATUS-OK
# "a live, reproduced instance: H23" — status word FIRST, hole id second. The second order  HOLE-STATUS-OK
# was missing until 2026-08-04 and two stale claims were sitting in it: VC_BRIDGE_REGISTER.md
# still called H23 "a live, reproduced instance" and declared every row's Assumes clause
# "currently false in the presence of loop invariants", and ROADMAP.md still called H24 "the
# live problem". Both had been false for a day, in the two documents most likely to be read
# as authoritative about what is broken.
#
# The lesson generalises past this gate: a prose check written against one phrasing tests
# that phrasing, not the claim. Widen on evidence, which is what this is.
#
# But widen CAREFULLY. The first version of the reverse pattern used bare `live|open`, and
# both words are heavily overloaded in this repo: "live at the loop boundary" is liveness
# analysis, and "the last open half of H1" describes a decision's scope. It produced four
# false positives across CHANGELOG.md and CALLABLE_VALUES_AND_CAPABILITIES.md. A gate whose
# failures are usually noise gets ignored, which costs more than the drift it catches — so
# the reverse direction matches only words that assert a hole is CURRENTLY defective
# (`reproduced`, `unfixed`, `live problem/instance/defect/counterexample`) and drops bare
# `open` entirely. Both stale claims that motivated this are still caught.  HOLE-STATUS-OK
VIOL2=0
for h in $CLOSED_HOLES; do
  HITS="$( { grep -rniE "\b$h\b[^.]{0,40}(is |remains |stays )(still )?(OPEN|unfixed|not fixed|reproduced)" $SCAN 2>/dev/null;
             grep -rniE "(reproduced|unfixed|live (problem|instance|defect|counterexample))[^.]{0,60}\b$h\b" $SCAN 2>/dev/null; } \
          | grep -viE "(was|were|had been|used to be|previously|formerly|no longer|CLOSED)" \
          | grep -v "HOLE-STATUS-OK" | sort -u || true)"
  if [ -n "$HITS" ]; then
    VIOL2=1
    no "$h is CLOSED in $KH but claimed still open elsewhere:"
    printf '%s\n' "$HITS" | head -4 | sed 's/^/         /'
  fi
done
[ "$VIOL2" -eq 0 ] && ok "no file claims a CLOSED hole is still open"

echo ""
echo "HOLE-STATUS-CONSISTENCY: PASS=$PASS  FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
