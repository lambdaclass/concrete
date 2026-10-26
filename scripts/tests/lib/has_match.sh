#!/usr/bin/env bash
# has_match [GREP_OPTION...] PATTERN: does the input contain a line grep selects? (bug 283)
#
#   has_match -F "E0217" <<<"$OUT"            # captured text: a here-string, no producer process
#   "$COMPILER" f.con --report x | has_match "status: proved"     # a streaming producer
#
# It answers what `grep -q` answers, without grep -q's early exit. `grep -q` stops reading at its
# first match; a producer still writing into the pipe then dies of SIGPIPE (status 141), and under
# `set -o pipefail` the pipeline reports that death instead of grep's 0. A positive leg then FAILs
# although the pattern is there, and a negated leg (`... && no || ok`) passes although the forbidden
# text is there. has_match reads ALL of its input before it answers (`grep -c`, whose output is a
# capture, never /dev/null, which GNU grep would treat like -q), so the producer always finishes
# writing and pipefail sees only real failures: a producer that fails still fails the pipeline.
#
# Status: 0 a line matched; 1 no line matched. Anything else is no answer at all: a grep error (an
# invalid pattern, an unreadable input) or an option that would stop grep early or replace its
# answer (-q, -m, -l, -L, -c and their long forms, anywhere in the arguments: GNU grep reads options
# after the pattern too) prints the reason and stops the script. From the script's own shell that
# is `exit 2`. From a pipeline stage or a substitution, a subshell whose status a negated leg would
# read as "absent", it is SIGKILL to the script (status 137): a signal the script cannot trap and
# carry on past, so neither polarity of leg can turn the error into a verdict.
#
# check_gate_hygiene.sh refuses a pipe into a short-circuiting grep (`| grep -q`, `-m`, `-l`, `-L`)
# and a `| head` feeding has_match, and proves every script that calls has_match loads this file.

has_match() {
  local _hm_a _hm_n _hm_rc _hm_c _hm_j _hm_take=0 _hm_opts=1
  for _hm_a in "$@"; do
    if [ "$_hm_take" = 1 ]; then _hm_take=0; continue; fi   # an option's separate argument
    [ "$_hm_opts" = 1 ] || continue
    case "$_hm_a" in
      --) _hm_opts=0 ;;
      --quiet|--silent|--count|--max-count|--max-count=*|--files-with-matches|--files-without-match)
        _has_match_fail "option '$_hm_a' is not a whole-input match" "$@" ;;
      --regexp|--file|--context|--after-context|--before-context|--binary-files|--devices|\
--directories|--label|--include|--exclude|--exclude-from|--exclude-dir|--group-separator)
        _hm_take=1 ;;
      --*) ;;
      -?*)
        # A short-option cluster: letters until one that takes an argument, which is the rest of
        # the word or, when the word ends there, the next word.
        for (( _hm_j=1; _hm_j<${#_hm_a}; _hm_j++ )); do
          _hm_c="${_hm_a:_hm_j:1}"
          case "$_hm_c" in
            [qmlLc]) _has_match_fail "option '-$_hm_c' is not a whole-input match" "$@" ;;
            [efABCdD]) [ $((_hm_j + 1)) -eq ${#_hm_a} ] && _hm_take=1; break ;;
          esac
        done ;;
    esac
  done
  # Status read through && || so an ERR trap or errexit in the caller does not act on grep's 1 here.
  _hm_n="$(grep -c "$@")" && _hm_rc=0 || _hm_rc=$?
  case "$_hm_rc" in
    0|1) return "$_hm_rc" ;;
  esac
  _has_match_fail "grep exited $_hm_rc" "$@"
}

_has_match_fail() {
  local why="$1"; shift
  printf 'has_match: %s (grep arguments:' "$why" >&2
  printf ' %q' "$@" >&2
  printf '); stopping %s with no verdict\n' "${0##*/}" >&2
  # A pipeline stage or a substitution is a subshell: its exit would only end that stage, and the
  # leg would read the status as "no match". Stop the script itself, with a signal it cannot trap.
  if [ "${BASH_SUBSHELL:-0}" -gt 0 ]; then kill -s KILL "$$" 2>/dev/null; fi
  exit 2
}

# Exported, so a check run as `bash -c "..."` (check_review_tool.sh's `check`) can call it too.
export -f has_match _has_match_fail
