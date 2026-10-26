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
# Status: 0 a line matched; 1 no line matched. A grep error (status 2: an invalid pattern, an
# unreadable input) is neither: it prints the error and stops the whole script with status 2, from
# a pipeline stage or a substitution too, so no leg of either polarity can read it as an answer.
#
# Options are grep's, minus the ones that stop early or replace the answer: -q, -m, -l, -L, -c and
# their long forms are refused (handled as a grep error). check_gate_hygiene.sh refuses a
# pipe into a short-circuiting grep (`| grep -q`, `-m`, `-l`, `-L`) and a `| head` before a match,
# and proves every script that calls has_match loads this file.

has_match() {
  local _hm_a _hm_n _hm_rc
  for _hm_a in "$@"; do
    case "$_hm_a" in
      --) break ;;
      --quiet|--silent|--max-count*|--files-with*|--count)
        _has_match_fail "option '$_hm_a' is not a whole-input match" "$@" ;;
      --*) ;;
      -*[qmlLc]*) _has_match_fail "option '$_hm_a' is not a whole-input match" "$@" ;;
      -*[efABC]) break ;;   # the next word is this option's argument; the rest is not inspected
      -*) ;;
      *) break ;;
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
  # leg would read status 2 as "no match". Stop the script itself.
  if [ "${BASH_SUBSHELL:-0}" -gt 0 ]; then kill -s TERM "$$" 2>/dev/null; fi
  exit 2
}

# Exported, so a check run as `bash -c "..."` (check_review_tool.sh's `check`) can call it too.
export -f has_match _has_match_fail
