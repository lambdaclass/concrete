#!/usr/bin/env bash
# THE SUPERVISOR'S DECISION, AS A PURE FUNCTION.
#
# A process cannot safely publish a verdict about its own exit. The campaign therefore runs as a
# child that may only propose a candidate record, and a supervisor that observes the child's exit,
# re-reconciles the tree itself, and installs the authoritative artifact. This file holds the part
# that DECIDES, separated from the part that spawns and writes, for one reason: a decision embedded
# in a 2,000-line driver can only be tested by running a full campaign, and a control that expensive
# does not get run. Here it is a pure string-in, string-out function that a gate can attack directly
# with hostile inputs.
#
# PURE: no filesystem writes, no process state, no globals. It reads one candidate file and returns
# a refusals string. Everything it needs is an argument, so a test can construct any situation —
# including ones that are hard to produce for real, like a child that exits zero after corrupting the
# tree.
#
# Empty output means "nothing refused". Any non-empty output means qualification must be denied.

# The keys that must be present for a candidate to be a record at all. A truncated file parses as a
# valid record with missing fields, and every one of these is load-bearing for what gets published.
# THE DISPOSITION FIELDS ARE MANDATORY. Treating a MISSING survived/invalid/could_not_apply as zero
# is fail-open in the one direction that matters: a truncated record loses exactly the fields that
# would have denied qualification, and then qualifies.
CAMPAIGN_CANDIDATE_KEYS="completed mode discovered selected executed reported killed invalid survived could_not_apply integrity_ok qualified"

# supervisor_refusals <child_rc> <candidate-path> <head0> <head1> <tracked0> <tracked1> <untracked0> <untracked1>
supervisor_refusals() {
  local rc="$1" cand="$2" h0="$3" h1="$4" t0="$5" t1="$6" u0="$7" u1="$8"
  local out="" k

  # THE CHILD'S EXIT IS EVIDENCE, NOT A FORMALITY. This is the case the boundary exists for: the
  # child can print a verdict and then die, and only something outside it can notice.
  [ "$rc" = "0" ] || out="$out child_exit($rc)"

  if [ ! -s "$cand" ]; then
    out="$out candidate_missing"
  else
    # THE WHOLE RECORD IS DECODED AGAINST THE SCHEMA, before any qualification logic reads a field.
    # Per-key greps checked the handful of fields someone remembered; a decoder cannot be outgrown by
    # adding a field, because an undeclared key is refused rather than ignored.
    local dec; dec="$(decode_candidate "$cand")"
    [ -z "$dec" ] || out="$out candidate_schema($dec)"
  fi

  # THE SUPERVISOR'S OWN OBSERVATION, not a re-read of what the child claimed. These are captured
  # before spawning and after reaping, so a child that lied about its own reconciliation — or died
  # between reconciling and exiting — is still caught.
  [ "$h0" = "$h1" ] || out="$out supervisor_head_changed"
  [ "$t0" = "$t1" ] || out="$out supervisor_tracked_changed"
  [ "$u0" = "$u1" ] || out="$out supervisor_untracked_changed"

  # A TREE STATE THAT COULD NOT BE READ IS NOT AN UNCHANGED TREE. Comparing two unavailable values
  # for equality would agree with itself and publish.
  case "$h1$t1$u1" in
    *TREESTATE-UNAVAILABLE*) out="$out supervisor_tree_state_unavailable" ;;
  esac
  [ -n "$h1" ] && [ -n "$t1" ] && [ -n "$u1" ] || out="$out supervisor_tree_state_empty"

  printf '%s' "${out# }"
}

# candidate_incoherent <candidate-path>  -> reasons a candidate's OWN fields contradict qualified=1
#
# KEY PRESENCE IS NOT COHERENCE. Checking only that the fields exist and then trusting `qualified=1`
# accepts a record that contradicts itself: completed=0 with qualified=1, integrity_ok=0 with
# qualified=1, a single-family mode claiming full-campaign qualification, or counts that do not
# reconcile. A supervisor that exists BECAUSE it does not trust the child cannot then take the
# child's headline field at face value.
candidate_incoherent() { # candidate [expected-family-count] [expected-gate-count]
  local c="$1" expected="${2:-}" expgates="${3:-}" out="" v
  _f() { sed -n "s/^$1=//p" "$c" | head -1; }
  # A CONTRADICTION IS A CONTRADICTION AT qualified=0 TOO.
  #
  # Returning immediately for every unqualified candidate meant the exact shape this harness already
  # shipped once — mode=campaign with discovered=85 and selected=1, a single-family run wearing a
  # full-campaign label — stayed acceptable so long as it did not also claim qualification. But that
  # record is not a modest result; it is a false description of what ran, and it is the artifact a
  # reader consults. These checks therefore run unconditionally, and only the qualification-specific
  # ones below are gated on qualified=1.
  local _m _d _s _e _r
  _m="$(_f mode)"; _d="$(_f discovered)"; _s="$(_f selected)"
  _e="$(_f executed)"; _r="$(_f reported)"
  case "$_m" in
    campaign) [ "$_s" = "$_d" ] || out="$out campaign_mode_selected_subset($_s of $_d)" ;;
    single)   [ "$_s" = "1" ]   || out="$out single_mode_selected($_s)" ;;
    # A SHARD selects a subset — possibly empty — and never more than was discovered. Which subset is
    # the aggregator's question (it holds the plan); here only the shape can be wrong.
    shard)    case "$_s:$_d" in
                *[!0-9:]*|:*|*:) out="$out shard_mode_nonnumeric_selection($_s of $_d)" ;;
                *) [ "$_s" -le "$_d" ] || out="$out shard_mode_selected_exceeds_discovered($_s of $_d)" ;;
              esac ;;
    *)        out="$out unknown_mode($_m)" ;;
  esac
  # Reporting more than was executed is not a partial result, it is an invented one.
  #
  # EACH OPERAND IS TESTED SEPARATELY. Testing the CONCATENATION let an empty field through whenever
  # its partner was numeric — "" and "12" concatenate to "12" — and `[ "" -le 12 ]` is not false, it
  # is a bash usage error that writes to stderr and returns 2, so the `||` fired and the record was
  # refused with `reported_exceeds_executed(>12)`: the right verdict attributed to the wrong cause.
  # An absent field is already refused by name at decode; here it simply is not a comparison.
  _num() { case "${1:-}" in ''|*[!0-9]*) return 1 ;; *) return 0 ;; esac; }
  if _num "$_r" && _num "$_e"; then
    [ "$_r" -le "$_e" ] || out="$out reported_exceeds_executed($_r>$_e)"
  fi
  if _num "$_e" && _num "$_s"; then
    [ "$_e" -le "$_s" ] || out="$out executed_exceeds_selected($_e>$_s)"
  fi
  if [ "$(_f qualified)" != "1" ]; then printf '%s' "$out"; return 0; fi
  [ "$(_f completed)"    = "1" ]        || out="$out qualified_without_completed"
  [ "$(_f integrity_ok)" = "1" ]        || out="$out qualified_without_integrity"
  [ "$(_f mode)"         = "campaign" ] || out="$out qualified_in_$(_f mode)_mode"
  # EVERY SELECTED UNIT REPORTED, AND EVERY ONE OF THEM KILLED. These are the counts the artifact
  # itself publishes; if they do not reconcile, the headline is not describing this run.
  # STRING EQUALITY IS NOT RECONCILIATION. Comparing the counts as text lets five identical
  # non-numeric values — or five empty strings — satisfy every equality and qualify. Each must be a
  # number first, and a qualifying campaign must have discharged a POSITIVE number of families.
  local d s e r k v
  d="$(_f discovered)"; s="$(_f selected)"; e="$(_f executed)"; r="$(_f reported)"; k="$(_f killed)"
  # NEGATIVE, NONCANONICAL AND OVERSIZED VALUES ARE NOT NUMBERS EITHER. `-1` and `007` both survive a
  # naive digit test in one direction or the other, and a value too large to be a family count is a
  # corrupted field rather than a big campaign.
  for v in "$d" "$s" "$e" "$r" "$k"; do
    case "$v" in
      ''|*[!0-9]*) out="$out qualified_with_nonnumeric_counts"; break ;;
      0) ;;
      0*) out="$out qualified_with_noncanonical_count($v)"; break ;;
    esac
    [ "${#v}" -le 6 ] || { out="$out qualified_with_implausible_count($v)"; break; }
  done
  case "$out" in *nonnumeric*) ;; *)
    [ "$d" -gt 0 ] || out="$out qualified_with_zero_families"
    # SELF-AGREEMENT IS NOT ENOUGH. Five counts that agree with each other say nothing about whether
    # they describe the CORPUS: a candidate reporting 1/1/1/1/1 satisfies every internal equality
    # while eighty-four families went unexamined. The child compares its kill count against the
    # pinned inventory; a supervisor that did not was strictly LOOSER than the process it audits,
    # which makes it a rubber stamp rather than a check.
    if [ -n "$expected" ]; then
      [ "$d" = "$expected" ] || out="$out qualified_against_wrong_population($d vs pinned $expected)"
    else
      out="$out qualified_without_a_pinned_population"
    fi
    { [ "$d" = "$s" ] && [ "$s" = "$e" ] && [ "$e" = "$r" ]; } \
      || out="$out qualified_with_counts($d/$s/$e/$r)"
    [ "$k" = "$r" ] || out="$out qualified_with_unkilled($k/$r)"
    # THE FIELDS THAT WERE NEVER READ.
    #
    # Qualification reconciled the five headline counts and the disposition ledger and then ignored
    # everything else the record publishes — so a candidate could claim a fully killed 85-family
    # campaign while saying it ran zero families, wrote no evidence, split its kills into numbers
    # that do not add up, or proved no gates at all. A field the artifact publishes and nothing ever
    # reads is a field that can say anything.
    local fr ew kg kb bg gp
    fr="$(_f families_run)"; ew="$(_f evidence_written)"
    kg="$(_f killed_by_gate)"; kb="$(_f killed_by_build)"
    bg="$(_f baseline_gates_green)"; gp="$(_f gates_proven)"
    # Every family selected must have RUN, and every one that ran must have left a record.
    [ "$fr" = "$e" ] || out="$out qualified_with_families_run($fr vs executed=$e)"
    [ "$ew" = "$r" ] || out="$out qualified_with_evidence_written($ew vs reported=$r)"
    # A kill is attributed to the gate or to the build; the two routes must account for every kill.
    if _num "$kg" && _num "$kb"; then
      [ "$(( kg + kb ))" = "$k" ] \
        || out="$out qualified_with_kill_split($kg+$kb vs killed=$k)"
    else
      out="$out qualified_with_nonnumeric_kill_split($kg/$kb)"
    fi
    # NONEMPTY IS NOT A VALUE. Requiring only that these be non-blank accepted the production
    # shapes that mean the OPPOSITE of what qualification claims: `0/33` green baseline gates, or
    # `0/85` gates proven, are perfectly non-empty and describe a campaign that established
    # nothing. Both fields are published as `<n>/<m>`, so both are read as such and must be whole.
    # BOTH RATIOS ARE PARSED AS <n>/<m>, WITH BOTH PARTS REQUIRED TO BE NUMBERS.
    #
    # An earlier version compared the two halves as strings, so `x/x` and `0/0` both satisfied
    # "numerator equals denominator" — the first is not a ratio and the second describes a campaign
    # with nothing in it. And `85/junk/85` slipped through because `%%/*` and `##*/` ignore the
    # middle. One parser, used for both fields, that rejects anything which is not exactly two
    # numbers separated by one slash.
    _ratio() { # value -> "n m", or nothing if it is not a ratio
      case "$1" in
        */*/*|'') return 1 ;;
        */*) ;;
        *) return 1 ;;
      esac
      local _n="${1%%/*}" _m="${1#*/}"
      case "$_n" in ''|*[!0-9]*) return 1 ;; esac
      case "$_m" in ''|*[!0-9]*) return 1 ;; esac
      printf '%s %s' "$_n" "$_m"
    }
    # A GREEN BASELINE MEANS ALL OF THEM, AND AT LEAST ONE. Nothing was proved by zero gates.
    if _bgv="$(_ratio "$bg")"; then
      set -- $_bgv
      if [ "$2" = "0" ]; then out="$out qualified_with_no_baseline_gates"
      elif [ "$1" != "$2" ]; then out="$out qualified_with_baseline_gates_red($bg)"; fi
      # AND THE DENOMINATOR IS THE REAL GATE POPULATION. Self-agreement accepts `1/1` and `999/999`.
      # A single-family run legitimately baselines ONE gate — but qualification requires
      # mode=campaign, and a full campaign must have baselined every gate the inventory declares.
      [ -z "$expgates" ] || [ "$2" = "$expgates" ] \
        || out="$out qualified_with_baseline_gate_population($bg vs $expgates declared gates)"
    else
      case "$bg" in '') out="$out qualified_without_baseline_gates_green" ;;
                    *) out="$out qualified_with_unparsable_baseline_gates($bg)" ;; esac
    fi
    # GATES_PROVEN IS NOT REQUIRED TO BE COMPLETE — IT IS REQUIRED TO BE TRUE.
    #
    # Demanding numerator == denominator REJECTED a correct campaign. `gates_proven` is published as
    # killed_by_gate/N, and a family whose mutation is killed by the BUILD rather than by its gate is
    # a legitimate complete result: this driver's own header gives `78/81` with three build kills as
    # the example. A check that refuses the exact shape a qualifying campaign produces would have
    # blocked the result this whole program exists to reach. What must hold is that the published
    # ratio agrees with the counts published beside it — otherwise `85/85` beside killed_by_gate=82
    # qualifies on a number nothing produced.
    if _gpv="$(_ratio "$gp")"; then
      set -- $_gpv
      [ "$1" = "$kg" ] || out="$out qualified_with_gates_proven_disagreeing($gp vs killed_by_gate=$kg)"
      [ -z "$expected" ] || [ "$2" = "$expected" ] \
        || out="$out qualified_with_gates_proven_population($gp vs pinned $expected)"
      [ "$1" != "0" ] || out="$out qualified_with_zero_gates_proven"
    else
      case "$gp" in '') out="$out qualified_without_gates_proven" ;;
                    *) out="$out qualified_with_unparsable_gates_proven($gp)" ;; esac
    fi
    # THE REMAINING PUBLISHED COUNTS. families_declared must be the pinned population, and a
    # qualifying campaign cannot have failures.
    local fd fl
    fd="$(_f families_declared)"; fl="$(_f failed)"
    [ -z "$expected" ] || [ "$fd" = "$expected" ] \
      || out="$out qualified_with_families_declared($fd vs pinned $expected)"
    [ "$fl" = "0" ] || out="$out qualified_with_failures($fl)"
    # THE FIELDS NOTHING EVER READ. `refusals` and `evidence_dir` are mandatory in the schema and
    # were checked for PRESENCE only, so a record could qualify while publishing its own integrity
    # refusals — `qualified=1` beside `refusals= fatal_integrity_failure` — or point evidence_dir at
    # a different run's tree. A field the artifact publishes and no one reads can say anything.
    local rf ed rid
    rf="$(_f refusals)"; ed="$(_f evidence_dir)"; rid="$(_f run_id)"
    case "$rf" in
      ''|' '|'  ') ;;
      *) case "$rf" in *[!\ ]*) out="$out qualified_with_refusals($rf)" ;; esac ;;
    esac
    [ -n "$ed" ] || out="$out qualified_without_evidence_dir"
    # THE EVIDENCE DIRECTORY MUST BE EXACTLY THIS RUN'S, NOT MERELY END WITH ITS NAME.
    #
    # A suffix test accepts `foreign-prefix/<run_id>` and even `evil<run_id>` — the published pointer
    # could name a tree that is not the one reconciled, inside a record that qualifies. The path is
    # constructed the same way the producer constructs it and compared whole.
    [ -z "$rid" ] || [ "$ed" = ".mutation-evidence/$rid" ] \
      || out="$out qualified_with_foreign_evidence_dir($ed, expected .mutation-evidence/$rid)"
    # THE DISPOSITIONS MUST ACCOUNT FOR THE REPORTED FAMILIES. Checking each disposition is zero and
    # that killed equals reported leaves the ledger unbalanced if a family is reported under no
    # disposition at all; this is the identity that makes the four numbers describe one population.
    local iv sv ca sum
    iv="$(_f invalid)"; sv="$(_f survived)"; ca="$(_f could_not_apply)"
    case "$iv$sv$ca" in
      ''|*[!0-9]*) ;;   # the per-field checks below name the offender
      *) sum=$(( k + iv + sv + ca ))
         [ "$sum" = "$r" ] || out="$out qualified_with_unbalanced_ledger($k+$iv+$sv+$ca=$sum vs reported=$r)" ;;
    esac
  esac
  # A MISSING DISPOSITION IS NOT A ZERO ONE. It is absent evidence, and absence must not qualify.
  for v in survived invalid could_not_apply; do
    case "$(_f $v)" in
      0) ;;
      '') out="$out qualified_without_$v" ;;
      *[!0-9]*) out="$out qualified_with_nonnumeric_$v" ;;
      *) out="$out qualified_with_$v($(_f $v))" ;;
    esac
  done
  printf '%s' "${out# }"
}

# supervisor_qualification <candidate-path> <refusals>  -> the qualified= line to publish
# Only a clean candidate, under a clean reconciliation, whose OWN fields justify it.
supervisor_qualification() { # candidate refusals [expected-family-count] [expected-gate-count]
  # THE SAME ARGUMENTS AS THE PRELIMINARY CALL, OR THIS IS A SECOND, LOOSER AUTHORITY.
  #
  # The supervisor asks candidate_incoherent twice: once to build its refusal list, and once here to
  # decide the published `qualified=` line. This call was omitting the gate count, so a `1/1`
  # baseline that the first call refused could still be written as qualified=1 by the second.
  # Production happened to be safe only because the first call's refusal was non-empty and short-
  # circuited this one — a coincidence of ordering, not a property.
  local cand="$1" refusals="$2" expected="${3:-}" expgates="${4:-}"
  if [ -n "$refusals" ] || [ ! -s "$cand" ]; then printf 'qualified=0'; return 0; fi
  grep -qE '^qualified=1$' "$cand" || { printf 'qualified=0'; return 0; }
  [ -z "$(candidate_incoherent "$cand" "$expected" "$expgates")" ] \
    && printf 'qualified=1' || printf 'qualified=0'
}

# evidence_root_digest <evidence-dir> -> digest over canonical (family id, record digest) pairs
#
# ONE PRODUCER, USED BY BOTH ROLES. The child computes this over the evidence it just published; the
# supervisor recomputes it after the child exits, over the same tree. If the two disagree, the bytes
# changed under the verdict — which is the race that "validate one object, publish a later-mutated
# object" describes. Two implementations could differ and agree with themselves, so there is one.
evidence_root_digest() { # evidence-dir -> digest, or a marker; nonzero on producer failure
  local root="$1" d n f out="" rec
  [ -d "$root" ] || { printf 'no-evidence-dir'; return 0; }
  # The run directory too: a linked evidence root means everything below it is somewhere else.
  [ -L "${root%/}" ] && { printf 'evidence-root-is-symlink'; return 3; }
  for d in "$root"/*/; do
    [ -e "$d" ] || continue
    n="$(basename "$d")"
    rec=""
    # THE LISTING IS PRODUCED FIRST, WITH ITS STATUS CHECKED. Inside the here-doc the substitution's
    # status is discarded, so an unreadable directory yielded an empty listing and a confident digest
    # over nothing — a hash that agrees with itself while describing no evidence at all.
    local listing
    # SYMLINKS ARE REFUSED, NOT SKIPPED.
    #
    # `-type f` does not match a symlink, but every consumer of this evidence — the `-s` tests, the
    # `sed` that reads a verdict, the census — FOLLOWS one. So a verdict or a transcript could be
    # replaced by a link to content outside the evidence tree, be read as authoritative by all of
    # them, and never appear in the digest that is supposed to detect exactly that. Evidence is
    # regular files; anything else is a refusal with its own status, not a silent omission.
    # INCLUDING THE DIRECTORY ITSELF. Testing only inside it missed the case where the family
    # DIRECTORY is a link: `-d`, the `*/` glob and `cd` all follow it, so `find .` starts in the
    # target and cannot see what it walked through to get there. The whole family's evidence could
    # then live outside the tree the root is supposed to describe.
    if [ -L "${d%/}" ]; then printf 'evidence-family-is-symlink'; return 3; fi
    if [ -n "$(cd "$d" 2>/dev/null && find . -type l -print -quit 2>/dev/null)" ]; then
      printf 'evidence-contains-symlink'; return 3
    fi
    listing="$(cd "$d" 2>/dev/null && find . -type f 2>/dev/null | LC_ALL=C sort)" || return 1
    # NAMES AND BOUNDARIES ARE PART OF THE EVIDENCE. Hashing concatenated CONTENTS alone let a
    # required transcript be RENAMED — gate.log to gate2.log, same sort position — without moving the
    # digest, even though the file the census demands had disappeared. Each file contributes its
    # path, its byte length and its own digest, so a rename, a truncation and a swap are all visible.
    while IFS= read -r f; do
      [ -n "$f" ] || continue
      local sz dg
      sz="$(wc -c < "$d$f" 2>/dev/null)" || return 1
      dg="$({ sha256sum "$d$f" 2>/dev/null || shasum -a 256 "$d$f"; } | cut -d' ' -f1)" || return 1
      [ -n "$dg" ] || return 1
      rec="$rec$f $sz $dg
"
    done <<EOF
$listing
EOF
    # A PRODUCER FAILURE IS NOT AN EMPTY DIRECTORY. Ignoring the status of find/sort/cat produced a
    # confident digest over partial or empty data — a hash that agrees with itself while describing
    # nothing. `find` failing here returns nonzero to the caller instead.
    [ -d "$d" ] || return 1
    # EACH FAMILY'S RECORDS ARE DIGESTED AS A UNIT, BEFORE ANY SORTING.
    #
    # The family name and its file records used to be appended as sibling lines and then GLOBALLY
    # sorted, which discards which record belonged to which family. Swapping the complete contents
    # of two family directories permuted the same multiset of lines and left the root unchanged, so
    # the digest attested that some evidence existed somewhere — not that THIS family was killed by
    # THIS transcript. Hashing the record block per directory makes the binding part of the digest:
    # a swap changes both family digests, so it changes the root.
    local fam_dg
    fam_dg="$(printf '%s' "$rec" | { sha256sum 2>/dev/null || shasum -a 256; } | cut -d' ' -f1)"
    [ -n "$fam_dg" ] || return 1
    out="$out$n $fam_dg
"
  done
  printf '%s' "$out" | LC_ALL=C sort | { sha256sum 2>/dev/null || shasum -a 256; } | cut -d' ' -f1
}

# family_set_digest <newline-separated family ids> -> canonical digest of the SET
#
# A COUNT CANNOT SEE A SUBSTITUTION. Pinning only the population size lets one family be swapped for
# another — the total stays 85 while a mutation silently leaves the corpus and a foreign one joins
# it. The identity of the set is what the campaign's evidence is about.
family_set_digest() {
  printf '%s' "$1" | grep -v '^$' | LC_ALL=C sort -u \
    | { sha256sum 2>/dev/null || shasum -a 256; } | cut -d' ' -f1
}

# gate_count_from_driver <driver-path> -> how many DISTINCT gates the inventory declares
#
# READ FROM THE SOURCE, like the family set. `baseline_gates_green` is published as <green>/<total>,
# and checking only that the two halves agree accepts `1/1` and `999/999` — a campaign that ran one
# baseline gate, or a corrupted number, both reading as a fully green baseline. The denominator has a
# correct value and it is derivable from the same `add` lines the families come from.
gate_count_from_driver() {
  grep -E '^add "' "$1" | awk '{print $4}' | tr -d '"' | grep -v '^$' | LC_ALL=C sort -u | grep -c .
}

# family_set_from_driver <driver-path> -> the declared family ids, one per line
#
# READ FROM THE SOURCE, not from the arrays the child built. The point is double entry: the child
# derives its set from what it RAN, the supervisor from what the driver DECLARES, and a disagreement
# means one of them is describing a different corpus. Two readings of the same array would agree with
# themselves and prove nothing.
family_set_from_driver() {
  grep -oE '^add "[^"]+"' "$1" 2>/dev/null | sed 's/^add "//; s/"$//'
}

# records_disposition_totals <evidence-dir> -> "killed invalid survived could_not_apply other"
#
# THE RECORDS ARE THE EVIDENCE; THE SUMMARY IS A CLAIM ABOUT THEM. Deriving the totals from the
# per-family records and comparing them with the summary is what stops a summary asserting that
# every family was killed while a record on disk says `survived`. A summary may never override a
# record, so the record side is computed first and independently.
records_disposition_totals() {
  local root="$1" d k=0 i=0 sv=0 c=0 o=0 disp
  [ -d "$root" ] || { printf '0 0 0 0 0'; return 0; }
  for d in "$root"/*/; do
    [ -e "$d" ] || continue
    [ "$(basename "$d")" != "_baseline" ] || continue
    [ -s "$d/verdict.txt" ] || { o=$((o+1)); continue; }
    # EXACTLY ONE disposition line, or the record does not state one.
    if [ "$(grep -cE '^disposition=' "$d/verdict.txt")" != "1" ]; then o=$((o+1)); continue; fi
    disp="$(sed -n 's/^disposition=//p' "$d/verdict.txt")"
    case "$disp" in
      killed)          k=$((k+1)) ;;
      invalid)         i=$((i+1)) ;;
      survived)        sv=$((sv+1)) ;;
      could_not_apply) c=$((c+1)) ;;
      *)               o=$((o+1)) ;;
    esac
  done
  printf '%s %s %s %s %s' "$k" "$i" "$sv" "$c" "$o"
}

# records_unkilled_or_unevidenced <evidence-dir> -> family ids that cannot support qualification
#
# QUALIFICATION IS A CLAIM ABOUT EVERY FAMILY. It requires each record to say `killed` AND to carry
# the transcripts that make that kill checkable — a gate kill needs its red, its restored green and
# its reapplied red, because without the green leg the red is not attributable to the mutation.
records_unkilled_or_unevidenced() {
  local root="$1" d n bad="" disp route
  [ -d "$root" ] || { printf 'no-evidence-dir'; return 0; }
  for d in "$root"/*/; do
    [ -e "$d" ] || continue
    n="$(basename "$d")"; [ "$n" != "_baseline" ] || continue
    disp="$(sed -n 's/^disposition=//p' "$d/verdict.txt" 2>/dev/null | head -1)"
    route="$(sed -n 's/^expected_route=//p' "$d/verdict.txt" 2>/dev/null | head -1)"
    [ "$disp" = "killed" ] || { bad="$bad $n(${disp:-no-disposition})"; continue; }
    if [ "$route" = "gate" ]; then
      for f in gate.log confirm_clean.log confirm_red.log; do
        [ -s "$d/$f" ] || bad="$bad $n(missing-$f)"
      done
    elif [ "$route" = "build" ]; then
      [ -s "$d/build.log" ] || bad="$bad $n(missing-build.log)"
    else
      bad="$bad $n(unknown-route-${route:-none})"
    fi
  done
  printf '%s' "${bad# }"
}

# ---------------------------------------------------------------------------------------------
# THE PUBLISHED SCHEMA, AND A DECODER THAT REFUSES ANYTHING OUTSIDE IT.
#
# Field-specific greps checked six keys and left the rest — evidence_root, run_id, head — free to be
# duplicated, blank or absent, and every reader took whichever value its parser reached first. A
# schema is the only form of this check that cannot be outgrown by adding a field: the decoder walks
# the record, and a key nobody declared is refused rather than ignored.
#
# DUPLICATES ARE DETECTED BEFORE ANY VALUE IS STORED. Parsing into a map first and checking after is
# how last-wins (or first-wins) silently resolves a contradiction that should have been fatal.
# LAUNCH_SCHEMA / decode_launch_report — the launcher report's contract, in one place.
#
# The supervisor used to decode this report inline and the child-process gate used to reimplement a
# looser version of the same thing, which meant the gate could pass while the consumer that actually
# gates publication rejected the identical bytes — or, worse, the reverse. There is one decoder now,
# and the gate calls it.
# EVERY PUBLISHED FIELD HAS A DECLARED AUTHORITY STATUS.
#
# A record is read as though all of its fields were equally established. They are not, and the
# difference is the whole of what a reader needs in order to know what a qualification is worth. The
# four sets below partition the published schema, and a control asserts the partition is exact — so
# a field cannot be added without someone deciding, in this file, what its status is.
#
#   OBSERVED — the supervisor computed or minted the value itself and compared. A child cannot alter
#   these without being caught.
#
#   CROSS-FIELD — cannot be observed after the run because the object is gone, but must be
#   internally consistent with something that was observed.
#
#   NON-AUTHORITATIVE — child-reported, unobservable, and checked only for well-formedness. A
#   well-formed value here does NOT establish the fact it names: `baseline_compiler_sha` is the
#   digest of a compiler that existed only inside a disposable workspace the supervisor never
#   entered and which is deleted before reconciliation, so it cannot show WHICH compiler ran. These
#   fields therefore support no qualification or provenance claim, and a control asserts that
#   qualification does not change when they do.
#
#   SEMANTIC — counts, dispositions and verdicts, reconciled against each other and against the
#   evidence on disk by candidate_incoherent and the supervisor's census.
CAMPAIGN_FIELDS_OBSERVED="executed_driver_sha preamble_driver_sha repo_driver_sha inventory_sha head tracked_sha untracked_sha run_id families_digest evidence_root evidence_dir"
CAMPAIGN_FIELDS_CROSSFIELD="workspace_head workspace_tracked_sha workspace_untracked_sha"
CAMPAIGN_FIELDS_NON_AUTHORITATIVE="baseline_compiler_sha compilers_tested"
CAMPAIGN_FIELDS_SEMANTIC="completed mode discovered selected executed reported killed invalid survived could_not_apply integrity_ok qualified families_declared families_run killed_by_gate killed_by_build failed evidence_written baseline_gates_green gates_proven refusals secs_total secs_copy secs_build secs_gate secs_other supervisor_refusals supervisor_child_exit candidate_incoherent"

# candidate_provenance <candidate> <exec-driver> <preamble> <repo-driver> <inventory> -> refusals
#
# NINE FIELDS DESCRIBED WHAT WAS TESTED AND NOTHING CHECKED ANY OF THEM.
#
# The supervisor verified the repository's head and tree digests and the family set, and took the
# child's word for the rest: which driver bytes executed, which inventory they came from, what the
# disposable workspace looked like, and which compiler was tested. Those are exactly the fields a
# reader consults to know WHAT a qualification is about, and a field nothing reads can say anything.
#
# Three kinds of check, and the difference between them is stated because it is the whole point:
#
#   OBSERVED — the supervisor computed the value itself and compares. The executed driver and the
#   preamble are digests IT minted before exec'ing the snapshot; the repository driver and inventory
#   it digests directly with the shared producer. A child cannot alter these without being caught.
#
#   CROSS-FIELD — the workspace is a disposable copy that no longer exists when this runs, so it
#   cannot be observed after the fact. What CAN be required is internal consistency: a faithful copy
#   of the repository at the same commit must report the same head and the same tree digests. This
#   is weaker than observation and is labelled as such.
#
#   SHAPE — the compiler digest and the tested-compilers note cannot be recovered at all once the
#   workspace is gone. They are required to be well formed and non-sentinel, which excludes the
#   `unknown` a failed measurement would leave behind, and nothing more is claimed.
candidate_provenance() {
  local c="$1" exec_d="$2" pre_d="$3" repo_d="$4" inv_d="$5" out="" v
  _p() { sed -n "s/^$1=//p" "$c" | head -1; }

  # OBSERVED — AND AN UNAVAILABLE OBSERVATION IS A REFUSAL, NOT A SKIP.
  #
  # The first version skipped a comparison whose expected value was empty. That made a hashing
  # failure silently DISABLE the check: delete `ts_inventory_digest` and the child's two snapshots
  # both become empty, agree with each other, satisfy the freeform schema, and the supervisor
  # compares nothing — the exact fail-open shape this tier exists to remove, introduced by the change
  # that claimed to remove it. An observation that could not be made is not evidence of agreement.
  local _obs
  for _obs in "executed_driver_sha:$exec_d" "preamble_driver_sha:$pre_d" \
              "repo_driver_sha:$repo_d" "inventory_sha:$inv_d"; do
    local _k="${_obs%%:*}" _want="${_obs#*:}"
    v="$(_p "$_k")"
    case "$_want" in
      ''|*TREESTATE-UNAVAILABLE*|*UNAVAILABLE*)
        out="$out ${_k}_unobservable(${_want:-<empty>})" ; continue ;;
    esac
    case "$v" in
      ''|*TREESTATE-UNAVAILABLE*) out="$out ${_k}_unpublished(${v:-<empty>})" ; continue ;;
    esac
    [ "$v" = "$_want" ] || out="$out ${_k}_mismatch($v vs $_want)"
  done

  # CROSS-FIELD: the workspace is a copy of this repository at this commit.
  local h wh t wt u wu
  h="$(_p head)";          wh="$(_p workspace_head)"
  t="$(_p tracked_sha)";   wt="$(_p workspace_tracked_sha)"
  u="$(_p untracked_sha)"; wu="$(_p workspace_untracked_sha)"
  [ "$wh" = "$h" ] || out="$out workspace_head_differs($wh vs $h)"
  [ "$wt" = "$t" ] || out="$out workspace_tracked_differs($wt vs $t)"
  [ "$wu" = "$u" ] || out="$out workspace_untracked_differs($wu vs $u)"

  # SHAPE: 32 hex characters, and never the sentinel a failed measurement leaves.
  v="$(_p baseline_compiler_sha)"
  case "${#v}:$v" in
    32:*[!0-9a-f]*) out="$out baseline_compiler_not_hex($v)" ;;
    32:*) ;;
    *) out="$out baseline_compiler_malformed($v)" ;;
  esac
  # A declared enumeration, so a new mode has to be added deliberately rather than appear in a record.
  v="$(_p compilers_tested)"
  case "$v" in
    per-family-rebuilds|single-baseline) ;;
    *) out="$out compilers_tested_unrecognised($v)" ;;
  esac
  printf '%s' "$out"
}

# candidate_run_binding <candidate> <expected-run-id> <observed-head> -> refusals
#
# THE RECORD MUST DESCRIBE THE RUN THAT WAS JUST SUPERVISED. Every reconciliation the supervisor
# performs uses the candidate's OWN run id to locate the evidence it then checks, so a stale
# candidate selects its own old evidence, reconciles perfectly against it, and answers on behalf of
# a child that has only just exited. Self-agreement is not the property wanted. This lived inline in
# the driver, where no gate could reach it; a refusal nothing can test is a refusal nothing can
# prove is still there.
candidate_run_binding() {
  local c="$1" want_run="$2" want_head="$3" out="" v
  [ -s "$c" ] || { printf ' candidate_missing'; return 0; }
  v="$(sed -n 's/^run_id=//p' "$c" | head -1)"
  [ "$v" = "$want_run" ] || out="$out candidate_from_other_run($v vs $want_run)"
  v="$(sed -n 's/^head=//p' "$c" | head -1)"
  [ "$v" = "$want_head" ] || out="$out candidate_head_mismatch($v vs $want_head)"
  # An unreadable observation is not a matching one; comparing two sentinels would agree with itself.
  case "$want_head" in
    ''|*TREESTATE-UNAVAILABLE*) out="$out supervisor_head_unreadable($want_head)" ;;
  esac
  printf '%s' "$out"
}

# group_state_permits_publication <process-group-state> -> 0 (may publish) / 1 (must not)
#
# ONLY A PROVEN-EMPTY GROUP. The launcher distinguishes four outcomes and a boolean would collapse
# them: `permission_denied` means the group EXISTS but is not ours to signal, and `error:<n>` means
# the question was not answered at all. Neither is absence. This was an inline `case` in the driver
# and a gate that re-stated the enumeration beside it, so the gate's controls could pass with the
# driver's refusal deleted — they were comparing strings, not exercising a decision.
group_state_permits_publication() {
  case "${1:-}" in
    empty) return 0 ;;
    *)     return 1 ;;
  esac
}

# supervisor_must_hold_lock <process-group-state> -> 0 (hold) / 1 (release)
#
# REFUSING TO PUBLISH WAS ONLY HALF THE RESPONSE. The supervisor released the repository lock on
# every exit path, so its own conclusion — "something from this run may still be writing the tree" —
# was immediately followed by admitting the next run. Only a PROVEN-empty group releases: the other
# three states are "still running", "exists but not ours to signal", and "the question was not
# answered", and none of those is absence.
# `no_child_launched` is its own answer, not a fall-through. The trap that consults this is
# installed BEFORE the child is spawned, so it fires on early failures — cannot stage the report,
# cannot load a library — where no campaign process group has ever existed. Letting those land in
# the default would strand the lock on exactly the failures that left nothing running, and a lock
# stranded by a clean refusal is indistinguishable to the next operator from one stranded by a
# crash. Every state is named; there is no default.
supervisor_must_hold_lock() {
  case "${1:-}" in
    empty|no_child_launched) return 1 ;;
    nonempty|permission_denied|error:*|*) return 0 ;;
  esac
}

LAUNCH_SCHEMA="protocol_version run_id child_rc child_signalled child_signal process_group_state pgid"
LAUNCH_SCHEMA_NUMERIC="child_rc child_signalled child_signal pgid"

# decode_launch_report <report> <expected-run-id> <launcher-rc> -> refusals; empty means usable
decode_launch_report() {
  local rep="$1" want_run="$2" lrc="${3:-0}" out="" line key n k
  # THE LAUNCHER'S OWN EXIT STATUS IS PART OF THE REPORT. It was recorded and then only printed in
  # the failure message, so a launcher that died after writing a plausible file was read as a clean
  # run. A launcher that did not exit zero has not answered the question it was asked.
  case "$lrc" in
    0) ;;
    *) out="$out launcher_exit($lrc)" ;;
  esac
  [ -s "$rep" ] || { printf '%s report_absent_or_empty' "$out"; return 0; }
  # EVERY LINE MUST BE A DECLARED ASSIGNMENT. The old scan extracted `^[a-z_]*=`, which simply does
  # not match `PGID=0` or `garbage`: an injected uppercase or malformed line was not an unknown key,
  # it was invisible. Anything that is not a declared lowercase key is refused by name.
  while IFS= read -r line || [ -n "$line" ]; do
    case "$line" in
      '') out="$out blank_line"; continue ;;
      *=*) key="${line%%=*}" ;;
      *) out="$out malformed_line"; continue ;;
    esac
    case " $LAUNCH_SCHEMA " in
      *" $key "*) ;;
      *) out="$out undeclared_key($key)" ;;
    esac
  done < "$rep"
  for k in $LAUNCH_SCHEMA; do
    n="$(grep -cE "^$k=" "$rep" 2>/dev/null || true)"
    [ "$n" = "1" ] || out="$out $k($n)"
  done
  local proto rid crc csig cnum gstate pgid
  proto="$(sed -n 's/^protocol_version=//p' "$rep" | head -1)"
  rid="$(sed -n 's/^run_id=//p' "$rep" | head -1)"
  crc="$(sed -n 's/^child_rc=//p' "$rep" | head -1)"
  csig="$(sed -n 's/^child_signalled=//p' "$rep" | head -1)"
  cnum="$(sed -n 's/^child_signal=//p' "$rep" | head -1)"
  gstate="$(sed -n 's/^process_group_state=//p' "$rep" | head -1)"
  pgid="$(sed -n 's/^pgid=//p' "$rep" | head -1)"
  [ "$proto" = "1" ] || out="$out unsupported_protocol($proto)"
  [ "$rid" = "$want_run" ] || out="$out stale_run_id($rid)"
  for k in $LAUNCH_SCHEMA_NUMERIC; do
    case "$(sed -n "s/^$k=//p" "$rep" | head -1)" in
      ''|*[!0-9]*) out="$out noncanonical($k)" ;;
    esac
  done
  case "$csig" in 0|1) ;; *) out="$out child_signalled_not_boolean($csig)" ;; esac
  case "$csig:$cnum" in
    0:0) ;;
    1:0) out="$out signal_fields_incoherent(signalled_without_signal)" ;;
    0:*) out="$out signal_fields_incoherent(signal_without_signalled)" ;;
    1:*)
      # A SIGNALLED CHILD'S STATUS IS 128+N, AND THE TWO FIELDS MUST SAY SO. Left unchecked, a report
      # could carry child_signal=9 beside child_rc=0 and a consumer reading the status alone would
      # conclude the campaign exited cleanly.
      # BOTH operands must be numbers before the arithmetic. `$(( 128 + cnum ))` on a non-numeric
      # cnum resolves it as a variable NAME, yields 128, and reports a mismatch against a number
      # that was never in the record.
      case "$crc:$cnum" in
        *[!0-9:]*|:*|*:) ;;
        *) [ "$crc" = "$(( 128 + cnum ))" ] \
             || out="$out signal_status_mismatch(rc=$crc signal=$cnum)" ;;
      esac ;;
  esac
  # The state is an enumeration; anything else is not an answer, and `empty` is the only one that
  # permits publication, so an unrecognised value must never reach that comparison unchallenged.
  case "$gstate" in
    empty|nonempty|permission_denied|error:*) ;;
    *) out="$out unknown_process_group_state($gstate)" ;;
  esac
  printf '%s' "$out"
}

CAMPAIGN_SCHEMA_NUMERIC="completed discovered selected executed reported killed invalid survived could_not_apply integrity_ok qualified secs_total secs_copy secs_build secs_gate secs_other families_declared families_run killed_by_gate killed_by_build failed evidence_written"
CAMPAIGN_SCHEMA_FREEFORM="mode baseline_gates_green gates_proven evidence_root evidence_dir run_id head executed_driver_sha preamble_driver_sha repo_driver_sha inventory_sha tracked_sha workspace_tracked_sha workspace_head workspace_untracked_sha baseline_compiler_sha compilers_tested untracked_sha refusals families_digest"

# THE SUPERVISOR'S OWN FIELDS ARE NOT THE CHILD'S TO PROVIDE. These are appended at publication, so
# requiring them of a CANDIDATE rejects every legitimate one — which is exactly what the first
# version of this decoder did. The published artifact carries both sets; the candidate carries one.
CAMPAIGN_SCHEMA_SUPERVISOR="supervisor_refusals supervisor_child_exit candidate_incoherent"
CAMPAIGN_SCHEMA_NUMERIC_SUPERVISOR="supervisor_child_exit"

CAMPAIGN_SCHEMA="$CAMPAIGN_SCHEMA_NUMERIC $CAMPAIGN_SCHEMA_FREEFORM"
CAMPAIGN_SCHEMA_PUBLISHED="$CAMPAIGN_SCHEMA $CAMPAIGN_SCHEMA_SUPERVISOR"

# decode_candidate <path> -> refusals; empty means the record is well formed under the schema
decode_candidate() { # record [schema] — defaults to the CHILD schema
  local c="$1" schema="${2:-$CAMPAIGN_SCHEMA}" line key val out="" seen="" k
  [ -s "$c" ] || { printf 'record_absent_or_empty'; return 0; }
  while IFS= read -r line || [ -n "$line" ]; do
    # MALFORMED LINES ARE REFUSED, not skipped: a record containing anything but declared
    # assignments is not a record, and skipping is how a corrupted half-write reads as valid.
    case "$line" in
      '') out="$out blank_line" ; continue ;;
      *=*) ;;
      *) out="$out malformed_line(${line%%[!A-Za-z_]*}...)" ; continue ;;
    esac
    key="${line%%=*}"; val="${line#*=}"
    [ -n "$key" ] || { out="$out blank_key"; continue; }
    case "$key" in *[!a-z_]*) out="$out noncanonical_key($key)"; continue ;; esac
    # BEFORE ANY STORE. Checked against what has already been seen, so a duplicate is a refusal
    # rather than an overwrite.
    case " $seen " in *" $key "*) out="$out duplicate_key($key)"; continue ;; esac
    seen="$seen $key"
    case " $schema " in *" $key "*) ;; *) out="$out undeclared_key($key)"; continue ;; esac
    case " $CAMPAIGN_SCHEMA_NUMERIC $CAMPAIGN_SCHEMA_NUMERIC_SUPERVISOR " in
      *" $key "*)
        case "$val" in
          ''|*[!0-9]*) out="$out noncanonical_number($key=$val)" ;;
          0) ;;
          0*) out="$out noncanonical_number($key=$val)" ;;
        esac ;;
    esac
  done < "$c"
  # EVERY DECLARED KEY MUST BE PRESENT. Absence is how a truncated record loses exactly the field
  # that would have denied it.
  for k in $schema; do
    case " $seen " in *" $k "*) ;; *) out="$out missing_key($k)" ;; esac
  done
  printf '%s' "${out# }"
}

# supervisor_reconcile_and_publish — THE WIRING, NOT JUST THE DECISIONS.
#
# Every function above decides one thing, and each is attacked directly by a gate. None of that
# proves the driver CONSULTS them. This body is what actually runs after the child exits: it decodes
# the launcher report, reconciles the candidate against the supervisor's own observations, digests
# the evidence, and either installs the authoritative artifact or refuses. It lived inline in the
# driver, where no gate could reach it, so deleting a refusal's CALL SITE left every helper-level
# control green — a set of well-tested decisions nothing was obliged to ask.
#
# It is here so that check_campaign_supervisor.sh executes these exact lines, and so that a mutation
# neutering one of them has a gate to turn red. It exits rather than returning, because it is the
# supervisor's last act; a caller that is not the supervisor must run it in a subshell.
#
# Reads the supervisor's state as globals rather than parameters — deliberately, because it was
# extracted verbatim from the driver and a parameter list would have been a rewrite disguised as a
# move. The names it needs are asserted present below, so a missing one is a refusal and not an
# empty string treated as an observation.
supervisor_reconcile_and_publish() {
  local _need _missing=""
  # `_cand` and `_child_rc` are deliberately NOT here: both are established inside this body, from
  # the launcher report. Requiring them was measured wrong on the first real run — the guard fired
  # on a correct campaign, which is the cheap direction for a guard to be wrong in.
  for _need in _launch_report _launch_rc _group_state RUN_ID ROOT_DIR EXPECTED_FAMILIES; do
    eval "[ -n \"\${$_need+set}\" ]" || _missing="$_missing $_need"
  done
  if [ -n "$_missing" ]; then
    echo "FATAL: supervisor_reconcile_and_publish called without:$_missing" >&2
    exit 2
  fi
  # THE REPORT IS DECODED BY THE SHARED DECODER, which is the same function the child-process gate
  # exercises. This block used to be an inline decoder with a gate-side reimplementation beside it;
  # two decoders of one format is one decoder too many, and the looser of the two is the one that
  # decides what "tested" means.
  _launch_bad="$(decode_launch_report "$_launch_report" "$RUN_ID" "$_launch_rc")"
    # NOTHING IS READ FROM A REPORT THAT HAS NOT BEEN ACCEPTED — and `_group_state` therefore keeps
    # the holding value set at launch rather than a figure taken from a record just refused.
    #
    # The fields were extracted first and the verdict consulted afterwards, so a malformed or
    # duplicated report still populated `_group_state` — and the EXIT trap, which decides whether to
    # hold the repository lock, then consulted a value taken from a record the supervisor had just
    # rejected. A duplicated `process_group_state` whose FIRST line said `empty` released the lock on
    # the strength of a report that was refused. `_group_state` stays unset on this path, so the trap
    # holds.
    if [ -n "$_launch_bad" ]; then
      rm -f "$_launch_report"
      echo "FATAL: the child launcher report is unusable:$_launch_bad (launcher rc=$_launch_rc)" >&2
      exit 2
    fi
  # EVERY FIELD THIS SUPERVISOR LATER READS IS EXTRACTED HERE, while the report still exists.
  # Extracting only three of them left `_child_sig` read at the signal refusal below with nothing
  # assigning it — `set -u` aborted the supervisor mid-run, so the campaign died after doing its
  # work and published nothing. Removing a decoder means removing the reads it fed, or keeping them.
  _child_rc="$(sed -n 's/^child_rc=//p' "$_launch_report" | head -1)"
  _child_sig="$(sed -n 's/^child_signalled=//p' "$_launch_report" | head -1)"
  _child_signum="$(sed -n 's/^child_signal=//p' "$_launch_report" | head -1)"
  _group_state="$(sed -n 's/^process_group_state=//p' "$_launch_report" | head -1)"
  _child_pgid="$(sed -n 's/^pgid=//p' "$_launch_report" | head -1)"
  rm -f "$_launch_report"

  _sup_head1="$(ts_head "$ROOT_DIR" 2>/dev/null)"
  _sup_tracked1="$(ts_tracked "$ROOT_DIR" 2>/dev/null)"
  _sup_untracked1="$(ts_untracked "$ROOT_DIR" 2>/dev/null)"

  _cand="$ROOT_DIR/.mutation-campaign-summary.candidate"
  _final="$ROOT_DIR/.mutation-campaign-summary"
  [ "${CONCRETE_MUT_PARTIAL:-0}" = "0" ] || _final="$_final.partial"

  # THE CANDIDATE IS SNAPSHOTTED BEFORE IT IS JUDGED. It is gitignored, so replacing it after
  # reconciliation would not move ts_untracked — the supervisor would validate one file and publish
  # another. Everything below reads the copy.
  _cand_snap="$(mktemp "${TMPDIR:-/tmp}/mutcand.XXXXXX" 2>/dev/null)" \
    || { echo "FATAL: supervisor cannot stage the candidate" >&2; exit 2; }
  : > "$_cand_snap"
  [ ! -s "$_cand" ] || cp "$_cand" "$_cand_snap" 2>/dev/null || : > "$_cand_snap"
  _cand="$_cand_snap"
  _sup_refusals="$(supervisor_refusals "$_child_rc" "$_cand" \
                    "$_sup_head0" "$_sup_head1" "$_sup_tracked0" "$_sup_tracked1" \
                    "$_sup_untracked0" "$_sup_untracked1")"
  # INCOHERENCE IS A REFUSAL, NOT MERELY A DOWNGRADE. It used to affect only the substituted
  # `qualified=` line, so a child that exited ZERO with a self-contradicting candidate got its record
  # published with qualified=0 and the supervisor exited 0 behind it — a passing process beside a
  # record that says the run did not qualify. That is precisely the PASS-versus-exit disagreement
  # this boundary exists to remove, reintroduced one layer up.
  # The gate population comes from the same source the families do — the driver's own `add` lines.
  # THE INVENTORY IS NAMED, NOT INHERITED FROM $0.
  #
  # These two producers read "$0" while this body lived inside the driver, where $0 WAS the driver.
  # Moving it into a library did not change $0 — it is still the top-level script — so production is
  # unaffected; but the dependency became invisible, and the first gate to call this function read
  # its OWN path and saw an inventory of zero families. A refusal about the harness, wearing the
  # words of a refusal about the campaign.
  #
  # So the driver names itself. Settable from the environment, which is acceptable here and nowhere
  # else: the supervisor sets it for itself before the call, and anything able to set the
  # supervisor's environment could already substitute this library wholesale. Unset or unreadable is
  # a refusal, not a fallback — a defaulted path is how "which inventory" quietly becomes "whichever
  # file happened to be there".
  if [ -z "${CAMPAIGN_DRIVER:-}" ] || [ ! -r "${CAMPAIGN_DRIVER:-}" ]; then
    echo "FATAL: CAMPAIGN_DRIVER is unset or unreadable: '${CAMPAIGN_DRIVER:-<unset>}'" >&2
    exit 2
  fi
  _sup_gates="$(gate_count_from_driver "$CAMPAIGN_DRIVER" 2>/dev/null || echo "")"
  _cand_incoh="$(candidate_incoherent "$_cand" "$EXPECTED_FAMILIES" "$_sup_gates" 2>/dev/null || echo candidate_unreadable)"
  # A CLEAN EXIT AND AN UNFINISHED RECORD CANNOT BOTH BE TRUE.
  #
  # `candidate_incoherent` deliberately stops short for unqualified records, so a child could exit
  # ZERO while its own candidate said completed=0, or integrity_ok=0, or carried refusals — and the
  # supervisor, which returns the child's status, exited zero too. Those are two statements about
  # one run and one of them is wrong; that is a refusal, not a partial result.
  if [ "$_child_rc" = "0" ]; then
    for _pair in "completed:1" "integrity_ok:1"; do
      _ck="${_pair%%:*}"; _cv="${_pair##*:}"
      _cg="$(sed -n "s/^$_ck=//p" "$_cand" | head -1)"
      [ "$_cg" = "$_cv" ] || _sup_refusals="$_sup_refusals clean_exit_with_$_ck($_cg)"
    done
    # `refusals` IS NOT CHECKED HERE, and the reason is a defect in the field rather than a
    # decision about it. The published value is `$REFUSALS$SCOPE_NOTES` — integrity refusals
    # CONCATENATED with scope annotations — so a correct single-family run publishes
    # `refusals= single_family_selected(85)` and exits zero. Refusing on non-emptiness therefore
    # rejected every correct partial run, which a run demonstrated within minutes of my writing it.
    # Splitting the field into two is a schema change with its own migration; until then this check
    # cannot be made sound, and asserting it anyway would be a gate that fires on correct work.
  fi
  [ -z "$_cand_incoh" ] || _sup_refusals="$_sup_refusals candidate_incoherent($_cand_incoh)"

  # THE EXACT FAMILY SET, NOT ITS SIZE. A one-for-one substitution keeps the count at 85 while a
  # mutation leaves the corpus and a foreign one takes its place. The supervisor derives the declared
  # set from the DRIVER SOURCE it is executing, the child derived its set from the inventory it
  # BUILT, and the two must agree — independent readings, not two looks at the same array.
  _sup_fams="$(family_set_from_driver "$CAMPAIGN_DRIVER")"
  _sup_famn="$(printf '%s\n' "$_sup_fams" | grep -cv '^$' || true)"
  _sup_famdig="$(family_set_digest "$_sup_fams")"
  _cand_famdig="$(sed -n 's/^families_digest=//p' "$_cand" | head -1)"
  if [ "$_sup_famn" != "$EXPECTED_FAMILIES" ]; then
    _sup_refusals="$_sup_refusals driver_declares_${_sup_famn}_families_not_$EXPECTED_FAMILIES"
  fi
  if [ -z "$_cand_famdig" ]; then
    _sup_refusals="$_sup_refusals candidate_names_no_family_set"
  elif [ "$_cand_famdig" != "$_sup_famdig" ]; then
    _sup_refusals="$_sup_refusals family_set_mismatch($_cand_famdig vs $_sup_famdig)"
  fi

  # THE EVIDENCE MUST BE THE EVIDENCE THAT WAS VALIDATED.
  #
  # .mutation-evidence/ is gitignored, so tree-state reconciliation cannot see it: a record or a gate
  # transcript could be removed or replaced between the child's census and this publication, and the
  # supervisor would install a qualifying summary describing evidence that no longer exists. The
  # child publishes an evidence ROOT — a digest over canonical (family id, record digest) pairs — and
  # the supervisor recomputes it here, over the same tree, after the child has exited. Disagreement
  # means the bytes changed under the verdict, whatever moved them.
  #
  # The recomputation uses the child's own function, loaded from the snapshot, so the two sides
  # cannot drift into computing different digests of the same directory.
  # THE CANDIDATE MUST BE THIS RUN'S CANDIDATE.
  #
  # Every reconciliation below trusted the candidate's own `run_id` to name the evidence directory to
  # check — so a stale candidate from an earlier run selected its own old evidence, reconciled
  # perfectly against it, and answered on behalf of a child that had just exited cleanly. The digest
  # agreeing with itself is not the property wanted; the property wanted is that the record describes
  # THE RUN THE SUPERVISOR JUST SUPERVISED. The run id is compared with the one the supervisor
  # minted, and the head the child observed with the head the supervisor observes.
  # The comparison itself lives in the library so a gate can attack it; see candidate_run_binding.
  # This site supplies the two observations only.
  # THE SAME PRODUCER THE CHILD USED. A second `git rev-parse` written out here would be a
  # reimplementation of `ts_head` — including its failure sentinel, which is the part that decides
  # what an unreadable repository compares as — and the two would drift apart exactly when it
  # mattered. This is the mistake that produced the CI extractor's 215-vs-208 disagreement.
  # THE OBSERVATION ALREADY TAKEN, not a third reading. `_sup_head1` was captured immediately after
  # the child was reaped and has already been reconciled against `_sup_head0`; reading HEAD again
  # here would introduce a third value that could disagree with the pair just checked, and the
  # binding would then be comparing the candidate against an observation nothing else validated.
  _sup_refusals="$_sup_refusals$(candidate_run_binding "$_cand" "$RUN_ID" "$_sup_head1")"

  # WHAT WAS TESTED IS CHECKED, NOT TAKEN ON THE CHILD'S WORD.
  #
  # The executed-driver and preamble digests are values THIS process minted before exec'ing the
  # snapshot, so comparing them is exact: a child that published anything else did not run what the
  # supervisor launched. The repository driver and inventory are digested here with the same shared
  # producer the child uses. The workspace and compiler fields cannot be observed after the run —
  # the workspace is deleted — so they are checked for internal consistency and shape, and
  # candidate_provenance labels which is which rather than implying they are all equally established.
  _sup_refusals="$_sup_refusals$(candidate_provenance "$_cand" \
    "${CONCRETE_MUT_DRIVER_SHA:-}" "${CONCRETE_MUT_PREAMBLE_SHA:-}" \
    "$(ts_driver_digest "$ROOT_DIR")" "$(ts_inventory_digest "$ROOT_DIR")")"

  # THE DIRECTORIES MUST BE THE FAMILIES. The family digest was compared candidate-to-driver, which
  # proves the candidate can name the right set — not that the evidence on disk IS that set. A
  # candidate could publish the correct digest beside eighty-five arbitrarily named killed
  # directories, and root and totals would all self-agree. The names on disk are digested with the
  # same producer and compared against the driver's declared set.
  _ev_dirs="$(cd "$ROOT_DIR/.mutation-evidence/$(sed -n 's/^run_id=//p' "$_cand" | head -1)" 2>/dev/null \
              && for _d in */; do [ -e "$_d" ] || continue; printf '%s\n' "${_d%/}"; done)"
  # SUBSET ALWAYS, EQUALITY ONLY FOR A FULL CAMPAIGN.
  #
  # The first version of this check compared the evidence set with the DECLARED set unconditionally,
  # which refused every correct partial run: a single-family run leaves one directory, and the
  # declared set has eighty-five. A check that fires on the runs it is meant to permit carries no
  # information — it is the same failure as the four liveness attempts. What is actually wrong is an
  # evidence directory that names NO declared family, and, for a full campaign, a set that is not
  # the whole population.
  if [ -n "$_ev_dirs" ]; then
    _ev_undeclared=""
    while IFS= read -r _evd; do
      [ -n "$_evd" ] || continue
      # `_baseline` is the RUN-LEVEL transcript, deliberately kept outside any family because it
      # belongs to the run rather than to one experiment. It is reserved by construction: no family
      # name may begin with an underscore, so this exclusion cannot hide a real family.
      case "$_evd" in _*) continue ;; esac
      printf '%s\n' "$_sup_fams" | grep -qxF -- "$_evd" \
        || _ev_undeclared="$_ev_undeclared $_evd"
    done <<EOF
$_ev_dirs
EOF
    [ -z "$_ev_undeclared" ] \
      || _sup_refusals="$_sup_refusals evidence_for_undeclared_families($_ev_undeclared )"
    if [ "${CONCRETE_MUT_PARTIAL:-0}" = "0" ]; then
      # family_set_digest takes its input as an ARGUMENT, not on stdin; piping to it would have
      # digested the empty string and agreed with any evidence tree that was also empty.
      _ev_setdig="$(family_set_digest "$(printf '%s\n' "$_ev_dirs" | grep -v '^_')")"
      [ "$_ev_setdig" = "$_sup_famdig" ] \
        || _sup_refusals="$_sup_refusals evidence_families_not_the_declared_set($_ev_setdig vs $_sup_famdig)"
    fi
  fi

  # THE CANDIDATE'S TREE DIGESTS ARE COMPARED WITH THE SUPERVISOR'S OBSERVATIONS.
  #
  # head was checked; tracked and untracked were not. The supervisor compared its OWN two readings
  # with each other and never with what the child published, so a record could carry stale or simply
  # false tree metadata and still be installed as authoritative — the fields describing what was
  # tested, unchecked, inside the artifact that attests to it.
  for _tpair in "tracked_sha:$_sup_tracked1" "untracked_sha:$_sup_untracked1"; do
    _tk="${_tpair%%:*}"; _tv="${_tpair#*:}"
    _tg="$(sed -n "s/^$_tk=//p" "$_cand" | head -1)"
    [ "$_tg" = "$_tv" ] || _sup_refusals="$_sup_refusals candidate_${_tk}_mismatch($_tg vs $_tv)"
  done

  _cand_root="$(sed -n 's/^evidence_root=//p' "$_cand" | head -1)"
  _sup_rootrc=0
  _sup_root="$(evidence_root_digest "$ROOT_DIR/.mutation-evidence/$(sed -n 's/^run_id=//p' "$_cand" | head -1)" 2>/dev/null)" || _sup_rootrc=$?
  [ "$_sup_rootrc" = "0" ] || _sup_refusals="$_sup_refusals evidence_root_unreadable($_sup_rootrc)"
  if [ -z "$_cand_root" ]; then
    _sup_refusals="$_sup_refusals evidence_root_absent"
  elif [ "$_cand_root" != "$_sup_root" ]; then
    _sup_refusals="$_sup_refusals evidence_changed_after_census($_cand_root->$_sup_root)"
  fi

  # NO DESCENDANT MAY STILL BE WRITING. A surviving child process can mutate evidence after this
  # check and before the artifact lands; the reconciliation above would then describe a tree that no
  # longer exists by the time anyone reads it.
  # THE RECORDS OUTRANK THE SUMMARY. Totals are derived from the per-family records on disk and
  # compared with what the candidate claims; a disagreement means the artifact is describing a run
  # other than the one that left this evidence.
  _ev_run_dir="$ROOT_DIR/.mutation-evidence/$(sed -n 's/^run_id=//p' "$_cand" | head -1)"
  set -- $(records_disposition_totals "$_ev_run_dir")
  _rk="$1"; _ri="$2"; _rs="$3"; _rc="$4"; _ro="$5"
  [ "$_ro" = "0" ] || _sup_refusals="$_sup_refusals records_with_unreadable_disposition($_ro)"
  for _pair in "killed:$_rk" "invalid:$_ri" "survived:$_rs" "could_not_apply:$_rc"; do
    _k="${_pair%%:*}"; _v="${_pair##*:}"
    _claim="$(sed -n "s/^$_k=//p" "$_cand" | head -1)"
    [ "$_claim" = "$_v" ] \
      || _sup_refusals="$_sup_refusals summary_record_disagreement($_k claimed=$_claim records=$_v)"
  done
  # ...and qualification additionally requires EVERY record to be a killed one carrying its causal
  # transcripts. This is checked only when qualification is claimed, so a legitimately unqualified
  # run is not refused for having survivors it already reported.
  if grep -qE '^qualified=1$' "$_cand"; then
    _unq="$(records_unkilled_or_unevidenced "$_ev_run_dir")"
    [ -z "$_unq" ] || _sup_refusals="$_sup_refusals qualified_with_unevidenced_records($_unq )"
  fi

  # NOT JUST IMMEDIATE CHILDREN. `pgrep -P $$` sees one generation, and by this point the campaign
  # child has been reaped — a surviving grandchild is reparented away and would never appear. What
  # matters is whether anything is still working THIS repository, so the check is by workspace and
  # by the driver's own snapshot, the same way the lock identifies its owner.
  # ONLY A PROVEN-EMPTY GROUP PERMITS PUBLICATION. The launcher distinguishes four outcomes and a
  # boolean would collapse them: `permission_denied` means the group EXISTS but is not ours to
  # signal, and `error:<n>` means the question was not answered at all. Neither is absence, and
  # treating either as empty would be the fail-open this check exists to remove.
  # AND THE LOCK IS NOT RELEASED WHEN WORK MAY SURVIVE.
  #
  # Refusing to publish was only half the response. The EXIT trap released the repository lock on
  # every path, so the supervisor's own conclusion — "something may still be writing this tree" —
  # was immediately followed by inviting the next run in. Holding the lock converts the refusal into
  # the exclusion it was asserting, and the operator is told exactly which pgid to inspect and what
  # to remove once it is gone. One repository writer at a time has to survive the case where the
  # previous writer did not stop.
  if ! group_state_permits_publication "$_group_state"; then
    case "$_group_state" in
      nonempty) _sup_refusals="$_sup_refusals campaign_group_not_empty(pgid=$_child_pgid)" ;;
      *)        _sup_refusals="$_sup_refusals campaign_group_unreadable($_group_state,pgid=$_child_pgid)" ;;
    esac
  fi
  # PUBLICATION ONLY AFTER THE CHILD'S ORIGINAL PROCESS GROUP IS EMPTY. Anything still in it can
  # write evidence after the census, which the artifact would then describe without having seen it.
  #
  # THIS IS NOT "NO DESCENDANTS REMAIN". A descendant that changes process group, or starts its own
  # session, outlives this check — measured, not assumed. That escape is out of the threat model:
  # the processes a campaign starts are its own gates, lake and the compiler, none of which leave
  # their group, and the hazard here is an accidental survivor rather than a child hiding from its
  # supervisor. The refusal and the published field both say process GROUP, so no reader infers a
  # containment guarantee that was never made.
  # A signalled child is a failure even if its group emptied cleanly.
  [ "$_child_sig" = "0" ] || _sup_refusals="$_sup_refusals child_terminated_by_signal"

  # PUBLISH. Copy the candidate, but the supervisor decides qualification: it is forced to 0 unless
  # the child claimed it AND the supervisor's own reconciliation is clean.
  _pub_ok=1
  _tmp="$(mktemp "$_final.XXXXXX" 2>/dev/null)" || { echo "FATAL: cannot stage the authoritative artifact" >&2; exit 2; }
  if [ -s "$_cand" ]; then
    # The qualified line is decided by the same library, not by a second rule here.
    sed "s/^qualified=.*/$(supervisor_qualification "$_cand" "$_sup_refusals" "$EXPECTED_FAMILIES" "$_sup_gates")/" "$_cand" > "$_tmp" || _pub_ok=0
  else
    # A FAILURE RECORD IS STILL A RECORD.
    #
    # This emitted four keys where the published schema declares forty-five, so the artifact the
    # supervisor installs when the child leaves no candidate — the case this boundary exists for —
    # could not be decoded by the very decoder that gates every other path. A second, looser
    # producer of the same artifact is the defect class this harness keeps finding; it was here, in
    # the failure path, where nobody looks.
    #
    # The mode was also computed as "${FAMILY:+single}${FAMILY:-campaign}", which concatenates BOTH
    # expansions: FAMILY=5 produced the mode `single5`, and an identity-selected run produced
    # `campaign` because FAMILY is unset even though the run is a single family. Partial-ness has
    # one fact, and this reads it like every other site.
    {
      for _sk in $CAMPAIGN_SCHEMA_NUMERIC;  do printf '%s=0\n' "$_sk"; done
      for _sk in $CAMPAIGN_SCHEMA_FREEFORM; do printf '%s=unavailable\n' "$_sk"; done
      # NOT the supervisor's own fields: they are appended below, for both branches, and emitting
      # them here as well produced two of each. The exactly-once check then refused publication — so
      # the repair that made this record complete also made it impossible to write. The candidate
      # branch does not carry them either; this branch must match it.
    } > "$_tmp.skel" || _pub_ok=0
    _mode="campaign"; [ "${CONCRETE_MUT_PARTIAL:-0}" = "0" ] || _mode="single"
    [ -z "${CONCRETE_MUT_SHARD:-}" ] || _mode="shard"
    sed -e "s|^mode=.*|mode=$_mode|" \
        -e "s|^refusals=.*|refusals= child_left_no_candidate|" \
        -e "s|^run_id=.*|run_id=$RUN_ID|" \
        -e "s|^head=.*|head=${_sup_head1:-unavailable}|" \
        "$_tmp.skel" > "$_tmp" || _pub_ok=0
    rm -f "$_tmp.skel"
  fi
  printf 'supervisor_refusals=%s
supervisor_child_exit=%s
' "${_sup_refusals:-none}" "$_child_rc" >> "$_tmp" || _pub_ok=0
  # EVERY WRITE IS CHECKED, not just the rename. A failed or truncated sed/printf — full disk, broken
  # pipe — could otherwise be installed as the authoritative artifact with exit 0.
  printf 'candidate_incoherent=%s\n' "${_cand_incoh:-none}" >> "$_tmp" || _pub_ok=0
  # EXACTLY ONE OF EACH. Presence alone lets a duplicated, contradictory field survive publication —
  # two `qualified=` lines, and every reader picks whichever its parser reaches first.
  for _k in completed mode qualified supervisor_refusals supervisor_child_exit candidate_incoherent; do
    [ "$(grep -cE "^$_k=" "$_tmp")" = "1" ] || _pub_ok=0
  done
  # AND THE WHOLE RECORD IS DECODED BEFORE IT IS INSTALLED.
  #
  # Counting six keys leaves the other thirty-nine unchecked, so a candidate refused for being
  # truncated, or for carrying duplicate or undeclared keys, was still COPIED into the authoritative
  # artifact — the file every reader trusts could be one the decoder that gates every other path
  # would reject. The supervisor already owns a decoder; it applies it to its own output.
  _pub_dec="$(decode_candidate "$_tmp" "$CAMPAIGN_SCHEMA_PUBLISHED" 2>/dev/null || echo decoder_failed)"
  if [ -n "$_pub_dec" ]; then
    echo "REFUSING TO INSTALL A MALFORMED AUTHORITATIVE RECORD:$_pub_dec" >&2
    _pub_ok=0
  fi
  if [ "$_pub_ok" != "1" ]; then
    rm -f "$_tmp" "$_cand_snap"
    echo "FATAL: the authoritative artifact could not be written completely" >&2
    exit 2
  fi
  # THE TREE IS RE-MEASURED IMMEDIATELY BEFORE INSTALL.
  #
  # Tree state was last read just after the child was reaped, and the evidence root just after that,
  # with all of the reconciliation in between. The repository lock excludes another campaign run — it
  # does not, and cannot, exclude an editor or any other process. So a qualifying artifact could be
  # installed already describing a tree that had moved since it was measured. This narrows the window
  # to the single rename below rather than closing it, which is the honest limit of a lock that only
  # binds participants; it is not claimed to be atomic against arbitrary writers.
  _sup_head2="$(ts_head "$ROOT_DIR" 2>/dev/null)"
  _sup_tracked2="$(ts_tracked "$ROOT_DIR" 2>/dev/null)"
  _sup_untracked2="$(ts_untracked "$ROOT_DIR" 2>/dev/null)"
  if [ "$_sup_head2" != "$_sup_head1" ] || [ "$_sup_tracked2" != "$_sup_tracked1" ] \
     || [ "$_sup_untracked2" != "$_sup_untracked1" ]; then
    rm -f "$_tmp" "$_cand_snap"
    echo "FATAL: the repository changed between reconciliation and publication." >&2
    echo "       Refusing to install a record describing a tree that has already moved." >&2
    echo "       The startup invalidation stands, so no earlier result reads as this run's." >&2
    exit 2
  fi
  mv "$_tmp" "$_final" 2>/dev/null || { rm -f "$_tmp"; echo "FATAL: cannot install the authoritative artifact" >&2; exit 2; }
  rm -f "$_cand_snap" "$ROOT_DIR/.mutation-campaign-summary.candidate" 2>/dev/null

  if [ -n "$_sup_refusals" ]; then
    echo "SUPERVISOR REFUSED QUALIFICATION:$_sup_refusals" >&2

    [ "$_child_rc" = "0" ] && exit 1 || exit "$_child_rc"
  fi

  exit "$_child_rc"
}
