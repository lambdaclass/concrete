#!/usr/bin/env bash
# Example output gate: every runnable example produces its recorded output,
# natively AND under the interpreter.
#
# check_examples.sh proves each example builds; this gate proves the programs
# DO what they claim. It replaces the old CI smoke step, which ran binaries with
# no input, failed only on crash signals, and skipped every example whose name
# matched *net*|*tcp*|*http*|*grep*|*toml*|*lox*|*project*.
#
# A UNIT is a `compile` entry of scripts/tests/example_manifest.txt (its .con
# path) or a project (its root directory with a trailing `/`, for the
# `project-build` entries). Every unit needs run rules in the same manifest:
#
#   @run <unit> <case> exit=<N> interp=<mode> oracle=<oracle> [ret=<V>]
#        [stdin=<file>] [stdout=empty] [-- <args...>]
#   @norun <unit> reason=<why>
#
# @run builds the unit, runs the binary in a scratch copy of the example
# directory (so fixtures resolve and nothing writes into the tree) with <args>
# and <stdin> (paths relative to the example directory; default stdin is
# /dev/null), and requires:
#   - stdout byte-identical to <dir>/expected/<case>.stdout (or empty with
#     stdout=empty, in which case that file must not exist);
#   - exit code exactly N.
# <dir> is the project root, else the source's directory (minus a trailing src/).
#
# interp=yes: `concrete <src> --interp -- <args...>` (with the same stdin, in its own
# scratch copy of the example directory) must print the expected stdout, followed by
# the line `<V>` when ret=V is given (the interpreter prints main's return value; the
# binary exits with it modulo 256, so V mod 256 must equal N), and must exit 0 with
# ret=, or N without it (a Unit main exits 0; a program that calls exit(N) exits N).
# A project unit is interpreted from its src/main.con with the project's whole module
# graph, std included, as `concrete build` compiles it.
# interp=fails:<marker>: the interpreter must FAIL with <marker> in its output.
# When it starts running the program and agrees with the binary, this gate fails
# until the rule is promoted to interp=yes; when it exits 0 with other output, that
# is a disagreement and fails too — a disagreement is never silently tolerated.
#
# oracle= says what independently checks the expected output (exit code and stdout). A golden
# recorded from the binary's own output proves only that the output does not change: bug 121's
# conlog golden blessed a wrong answer because the interpreter could not run conlog. So every
# @run rule declares one of:
#   oracle=interp               the interpreter agrees (requires interp=yes; enforced above);
#   oracle=spec:<path>[#anchor] derived without the compiler: by hand from the program's
#                               specification, or by an independent reference tool, with the
#                               derivation recorded at <path> (a repository file; #anchor, when
#                               given, must be one of its headings);
#   oracle=unverified:<reason>  no independent check yet ('~' for spaces). Allowed only when the
#                               interpreter does not run the case (interp=yes needs oracle=interp);
#                               each one is a tracked gap: the gate lists them and prints the count.
# A rule with no oracle, an unknown oracle, oracle=interp without interp=yes, a spec pointer that
# does not resolve, or an unverified oracle without a reason FAILS.
#
# Timeouts are INCOMPLETE, not failures (scripts/tests/lib/bounded.sh): a native, interpreter or
# reproducer run that exceeds its budget is re-run once alone with twice the budget; a second
# timeout is reported INCOMPLETE and the gate exits nonzero without calling the case wrong.
#
# Coverage is fail-closed: a unit with no rule, a rule for an unknown unit, a
# unit with both @run and @norun, a duplicate case, and an expected/*.stdout file
# no rule uses all FAIL.
#
# Negative controls (temporary manifest copies, one small unit) run at the end:
# a wrong exit code, a wrong interpreter value, a wrong stdout, interp=fails on a
# program the interpreter runs, a missing rule and an
# inconsistent ret= must each FAIL, as must a rule with no oracle, oracle=interp without
# interp=yes, an unverified oracle on a case the interpreter runs or without a reason, and a spec
# pointer to a missing file or heading; the real rule must pass, and a real unverified rule must
# pass and be counted. Eleven controls pin lib/bounded.sh: a command that prints and then hangs twice
# is incomplete with no output; a command that itself exits 124 is not incomplete; a run that
# times out once and then completes is recorded as recovered; the watchdog form reports INCOMPLETE
# and fails bounded_gate_status; a TERM-ignoring child, and a TERM-ignoring orphan whose parent
# died, are killed before bounded_run returns; a command's stderr can neither fake nor hide a
# timeout; a runner that reports nothing gives no output and is incomplete; budgets are decimal
# seconds (a fraction runs, a malformed one is a usage error); a cancelled runner kills the
# command's group.
#
# Environment (used by the controls): EXAMPLE_MANIFEST, EXAMPLE_OUTPUTS_ONLY
# (restrict coverage and runs to one unit), EXAMPLE_OUTPUTS_IS_CONTROL=1.
# EXAMPLE_VALGRIND=1 runs every native binary under valgrind memcheck (Linux CI):
# a memory error fails the run deterministically, where the bare binary may only
# crash sometimes (bug 121: conlog freed a corrupted pointer, SIGSEGV on one CI
# run in several, never on macOS; valgrind reported it on every run).

set -uo pipefail
source "$(dirname "${BASH_SOURCE[0]}")/lib/gate_env.sh" || exit 2

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"

COMPILER="$ROOT_DIR/.lake/build/bin/concrete"
MANIFEST="${EXAMPLE_MANIFEST:-scripts/tests/example_manifest.txt}"
ONLY="${EXAMPLE_OUTPUTS_ONLY:-}"
RUN_TIMEOUT=30
INTERP_TIMEOUT=120
VG=()
if [ "${EXAMPLE_VALGRIND:-0}" = 1 ]; then
  command -v valgrind >/dev/null 2>&1 || { echo "error: EXAMPLE_VALGRIND=1 but valgrind is not installed" >&2; exit 2; }
  # 98: a status no example expects, so a memory error can never pass as the program's exit.
  VG=(valgrind -q --error-exitcode=98 --leak-check=no)
  RUN_TIMEOUT=300
fi

[ -x "$COMPILER" ] || { echo "error: compiler not found at $COMPILER. Run 'lake build' first." >&2; exit 2; }
[ -f "$MANIFEST" ] || { echo "error: manifest not found at $MANIFEST" >&2; exit 2; }

TMP="$(mktemp -d)"
trap 'rm -rf "$TMP"' EXIT
BOUNDED_PARENT="$TMP"
# shellcheck source=lib/bounded.sh
source "$ROOT_DIR/scripts/tests/lib/bounded.sh"

PASS=0
FAIL=0
NORUN=0
INCOMPLETE=0
ORACLE_INTERP=0
ORACLE_SPEC=0
UNVERIFIED=0
unverified_list=""
incomplete_list=""
errors=""
ok() { PASS=$((PASS + 1)); echo "  ok   $1"; }
inc() { # a timed-out run: no observation, neither a pass nor a failure
  INCOMPLETE=$((INCOMPLETE + 1))
  incomplete_list="$incomplete_list\n  INCOMPLETE $1"
  echo "  INCOMPLETE $1"
}
# spec_pointer_ok <path[#anchor]>: the file exists and, with an anchor, one of its headings has
# that GitHub-style slug.
spec_pointer_ok() {
  local file="${1%%#*}" anchor=""
  case "$1" in *"#"*) anchor="${1#*#}" ;; esac
  [ -n "$file" ] && [ -f "$file" ] || return 1
  [ -z "$anchor" ] && return 0
  grep -E '^#+ ' "$file" | sed -E 's/^#+ +//' | tr 'A-Z' 'a-z' | sed -E 's/[^a-z0-9 _-]//g; s/ /-/g' | has_match -xF -- "$anchor"
}
no() {
  FAIL=$((FAIL + 1))
  errors="$errors\n  FAIL $1"
  echo "  FAIL $1"
}

project_root_of() {
  local dir
  dir=$(dirname "$1")
  while [ "$dir" != "." ] && [ "$dir" != "/" ]; do
    if [ -f "$dir/Concrete.toml" ]; then echo "$dir"; return 0; fi
    dir=$(dirname "$dir")
  done
  return 1
}

# --- 1. Units from the build-outcome lines. ---
UNITS="$TMP/units"; : > "$UNITS"
while read -r src outcome _; do
  case "$outcome" in
    compile) echo "$src" >> "$UNITS" ;;
    project-build)
      root=$(project_root_of "$src") || continue
      echo "$root/" >> "$UNITS" ;;
  esac
done < <(grep -v '^[[:space:]]*#' "$MANIFEST" | grep -v '^[[:space:]]*$' | grep -v '^[[:space:]]*@')
sort -u "$UNITS" -o "$UNITS"
if [ -n "$ONLY" ]; then
  grep -qxF -- "$ONLY" "$UNITS" || { echo "error: EXAMPLE_OUTPUTS_ONLY=$ONLY is not a unit" >&2; exit 2; }
  echo "$ONLY" > "$UNITS"
fi

RULES="$TMP/rules"
grep -E '^[[:space:]]*@(run|norun)[[:space:]]' "$MANIFEST" | sed 's/^[[:space:]]*//' > "$RULES" || true
if [ -n "$ONLY" ]; then
  awk -v u="$ONLY" '$2 == u' "$RULES" > "$RULES.only"; mv "$RULES.only" "$RULES"
fi
bad_rules=$(grep -vE '^@(run|norun)[[:space:]]' "$MANIFEST" | grep -E '^[[:space:]]*@' || true)
[ -n "$bad_rules" ] && no "unknown @ directive(s) in $MANIFEST: $(echo "$bad_rules" | awk 'NR<=3' | tr '\n' ';')"

base_of() { # unit -> example directory
  case "$1" in
    */) echo "${1%/}" ;;
    *) local d; d=$(dirname "$1"); case "$d" in */src) d=$(dirname "$d") ;; esac; echo "$d" ;;
  esac
}

# --- 2. Coverage. ---
echo "=== every runnable unit has run rules ==="
while IFS= read -r u; do
  nrun=$(awk -v u="$u" '$1 == "@run" && $2 == u' "$RULES" | wc -l | tr -d ' ')
  nnorun=$(awk -v u="$u" '$1 == "@norun" && $2 == u' "$RULES" | wc -l | tr -d ' ')
  if [ "$nrun" -eq 0 ] && [ "$nnorun" -eq 0 ]; then
    no "$u has no @run or @norun rule in $MANIFEST"
  elif [ "$nrun" -gt 0 ] && [ "$nnorun" -gt 0 ]; then
    no "$u has both @run and @norun rules"
  elif [ "$nnorun" -gt 1 ]; then
    no "$u has more than one @norun rule"
  fi
done < "$UNITS"
while read -r kind u _; do
  grep -qxF -- "$u" "$UNITS" || no "$kind names '$u', which is not a compile entry or a project root (a trailing / names a project) in $MANIFEST"
done < "$RULES"
dupcases=$(awk '$1 == "@run" {print $2 " " $3}' "$RULES" | sort | uniq -d)
[ -n "$dupcases" ] && no "duplicate @run case(s): $(echo "$dupcases" | tr '\n' ';')"
if [ -z "$ONLY" ]; then
  while IFS= read -r f; do
    d=$(dirname "$(dirname "$f")"); c=$(basename "$f" .stdout)
    used=0
    while read -r _ u case _; do
      [ "$(base_of "$u")" = "$d" ] && [ "$case" = "$c" ] && used=1
    done < <(awk '$1 == "@run"' "$RULES")
    [ "$used" = 1 ] || no "$f is not used by any @run rule (orphaned expectation)"
  done < <(find examples -path '*/expected/*.stdout' | LC_ALL=C sort)
fi

# --- 3. @norun rules need a reason. ---
while read -r _ u rest; do
  reason=$(printf '%s\n' "$rest" | sed -n 's/^.*reason=//p' | sed 's/^[[:space:]]*//')
  if [ -z "$reason" ]; then no "@norun $u has no reason="; else NORUN=$((NORUN + 1)); fi
done < <(awk '$1 == "@norun"' "$RULES")

# --- 4. Run every @run case. ---
echo "=== native and interpreter output of every @run case ==="
built=""   # units already built: "unit=binary" entries, newline-separated
binary_for() { # unit -> path of a fresh binary (built once per unit), or empty
  local u="$1" key b
  key=$(echo "$u" | tr '/.' '__')
  b="$TMP/bin_$key"
  if [ -x "$b" ]; then echo "$b"; return 0; fi
  case "$u" in
    */) (cd "${u%/}" && "$COMPILER" build -o "$b" > "$TMP/build_$key.log" 2>&1) ;;
    *) "$COMPILER" "$u" -o "$b" > "$TMP/build_$key.log" 2>&1 ;;
  esac
  [ -x "$b" ] && echo "$b"
}

n=0
while IFS= read -r line; do
  n=$((n + 1))
  # split "<head> -- <args>"
  head_part="$line"; args=""
  case "$line" in
    *" -- "*) head_part="${line%% -- *}"; args="${line#* -- }" ;;
  esac
  set -- $head_part
  u="$2"; case_name="${3:-}"; shift 3 2>/dev/null || true
  exit_want=""; ret=""; stdin_rel=""; stdout_empty=0; interp=""; oracle=""; oracle_kind=""
  for kv in "$@"; do
    case "$kv" in
      exit=*) exit_want="${kv#exit=}" ;;
      ret=*) ret="${kv#ret=}" ;;
      stdin=*) stdin_rel="${kv#stdin=}" ;;
      stdout=empty) stdout_empty=1 ;;
      interp=*) interp="${kv#interp=}" ;;
      oracle=*) oracle="${kv#oracle=}" ;;
      *) no "$u/$case_name: unknown field '$kv'"; continue 2 ;;
    esac
  done
  label="$u [$case_name]"
  if [ -z "$case_name" ] || ! [[ "$exit_want" =~ ^[0-9]+$ ]] || [ -z "$interp" ]; then
    no "$label: an @run rule needs a case name, exit=<N> and interp=<yes|fails:marker>"
    continue
  fi
  if [ -n "$ret" ] && ! [[ "$ret" =~ ^-?[0-9]+$ ]]; then no "$label: ret=$ret is not an integer"; continue; fi
  if [ -n "$ret" ] && [ $(( ( (ret % 256) + 256 ) % 256 )) -ne "$exit_want" ]; then
    no "$label: ret=$ret is inconsistent with exit=$exit_want (the binary exits with main's value modulo 256)"
    continue
  fi
  # The golden's independent check (see the header).
  case "$oracle" in
    "")
      no "$label: declares no oracle= (interp, spec:<path>[#anchor] or unverified:<reason>): an expected output counts only with an independent check"
      continue ;;
    interp)
      if [ "$interp" != yes ]; then
        no "$label: oracle=interp but interp=$interp: the interpreter does not check this golden; use oracle=spec:<pointer> or oracle=unverified:<reason>"
        continue
      fi
      oracle_kind=interp ;;
    spec:*)
      if ! spec_pointer_ok "${oracle#spec:}"; then
        no "$label: oracle=$oracle: the derivation pointer does not resolve (a repository file, and #anchor one of its headings)"
        continue
      fi
      oracle_kind=spec ;;
    unverified:*)
      if [ -z "${oracle#unverified:}" ]; then no "$label: oracle=unverified: needs a reason"; continue; fi
      if [ "$interp" = yes ]; then
        no "$label: oracle=unverified but interp=yes: interpreter agreement is enforced, so declare oracle=interp"
        continue
      fi
      oracle_kind=unverified ;;
    *) no "$label: oracle=$oracle is not interp, spec:<pointer> or unverified:<reason>"; continue ;;
  esac
  base=$(base_of "$u")
  exp="$base/expected/$case_name.stdout"
  if [ "$stdout_empty" = 1 ]; then
    [ -e "$exp" ] && { no "$label: stdout=empty but $exp exists — use one or the other"; continue; }
    : > "$TMP/expected_empty"; exp="$TMP/expected_empty"
  elif [ ! -f "$exp" ]; then
    no "$label: expected stdout $exp does not exist (or write stdout=empty)"
    continue
  fi
  if [ -n "$stdin_rel" ] && [ ! -f "$base/$stdin_rel" ]; then
    no "$label: stdin file $base/$stdin_rel does not exist"
    continue
  fi
  bin=$(binary_for "$u")
  if [ -z "$bin" ]; then
    no "$label: does not build"
    awk 'NR<=5' "$TMP"/build_"$(echo "$u" | tr '/.' '__')".log
    continue
  fi

  # Native: run in a scratch copy of the example directory.
  work="$TMP/work_$n"
  cp -R "$base" "$work"
  in="/dev/null"; [ -n "$stdin_rel" ] && in="$work/$stdin_rel"
  # shellcheck disable=SC2086
  # A clean environment: examples that read it (envcfg) must not see the caller's.
  (cd "$work" && bounded_run -i "$in" "$RUN_TIMEOUT" -- env -i PATH="$PATH" LC_ALL=C ${VG[@]+"${VG[@]}"} "$bin" $args > "$TMP/nat_$n.out" 2> "$TMP/nat_$n.err")
  rc=$?
  native_ok=1
  if bounded_is_incomplete "$rc"; then
    inc "$label: the native run timed out twice (${RUN_TIMEOUT}s, then alone): no observation"
    rm -rf "$work"
    continue
  elif [ "${#VG[@]}" -gt 0 ] && [ "$rc" -eq 98 ] && [ "$exit_want" -ne 98 ]; then
    no "$label: valgrind reports a memory error in the native run"
    grep '^==' "$TMP/nat_$n.err" | awk 'NR<=12'
    native_ok=0
  elif [ "$rc" -ne "$exit_want" ]; then
    no "$label: native exit $rc, expected $exit_want"
    awk 'NR<=3' "$TMP/nat_$n.err"
    native_ok=0
  fi
  if ! cmp -s "$exp" "$TMP/nat_$n.out"; then
    no "$label: native stdout differs from $exp"
    diff "$exp" "$TMP/nat_$n.out" | awk 'NR<=8'
    native_ok=0
  fi
  rm -rf "$work"

  # Interpreter.
  # Its own scratch copy: the interpreter reads the same fixtures and must not see what the
  # native run wrote.
  isrc="$ROOT_DIR/$u"
  case "$u" in */) isrc="$ROOT_DIR/${u%/}/src/main.con" ;; esac
  iwork="$TMP/iwork_$n"
  cp -R "$base" "$iwork"
  iin="/dev/null"; [ -n "$stdin_rel" ] && iin="$iwork/$stdin_rel"
  # shellcheck disable=SC2086
  (cd "$iwork" && bounded_run -i "$iin" "$INTERP_TIMEOUT" -- env -i PATH="$PATH" HOME="${HOME:-/}" LC_ALL=C "$COMPILER" "$isrc" --interp -- $args > "$TMP/int_$n.out" 2> "$TMP/int_$n.err")
  irc=$?
  rm -rf "$iwork"
  if bounded_is_incomplete "$irc"; then
    inc "$label: the interpreter timed out twice (${INTERP_TIMEOUT}s, then alone): no observation"
    continue
  fi
  iexit_want=0; [ -z "$ret" ] && iexit_want="$exit_want"
  case "$interp" in
    yes)
      cp "$exp" "$TMP/int_$n.want"
      [ -n "$ret" ] && echo "$ret" >> "$TMP/int_$n.want"
      # An interpreter refusal or trap (an `interp:` diagnostic) is never agreement, even when
      # its status happens to equal the program's expected exit status.
      if grep -q '^interp: ' "$TMP/int_$n.err"; then
        no "$label: the interpreter failed: $(grep '^interp: ' "$TMP/int_$n.err" | awk 'NR==1')"
      elif [ "$irc" -ne "$iexit_want" ]; then
        no "$label: interpreter exited $irc, expected $iexit_want"
        cat "$TMP/int_$n.err" "$TMP/int_$n.out" | awk 'NR<=3'
      elif ! cmp -s "$TMP/int_$n.want" "$TMP/int_$n.out"; then
        no "$label: interpreter output differs (expected stdout + main's value)"
        diff "$TMP/int_$n.want" "$TMP/int_$n.out" | awk 'NR<=8'
      elif [ "$native_ok" = 1 ]; then
        ORACLE_INTERP=$((ORACLE_INTERP + 1))
        ok "$label: native exit $exit_want + stdout, interpreter agrees"
      fi
      ;;
    fails:*)
      marker="${interp#fails:}"
      marker="${marker//\~/ }"   # '~' stands for a space (rules are whitespace-split)
      if [ -z "$marker" ]; then no "$label: interp=fails: needs a marker"; continue; fi
      # A recorded failure is the interpreter REFUSING (an `interp:` diagnostic). A run to
      # completion (exit 0, or the program's own exit status) means the rule must become
      # interp=yes, which then compares the output: a disagreement is filed, never kept here.
      if [ "$irc" -eq 0 ] || ! grep -q '^interp: ' "$TMP/int_$n.err"; then
        no "$label: interp=fails:$marker but the interpreter now runs it (exit $irc) — promote the rule to interp=yes"
      elif ! grep -qF -- "$marker" "$TMP/int_$n.out" "$TMP/int_$n.err"; then
        no "$label: the interpreter fails, but not with '$marker':"
        cat "$TMP/int_$n.err" "$TMP/int_$n.out" | awk 'NR<=3'
      elif [ "$native_ok" = 1 ]; then
        if [ "$oracle_kind" = spec ]; then
          ORACLE_SPEC=$((ORACLE_SPEC + 1))
          ok "$label: native exit $exit_want + stdout, derived at ${oracle#spec:} (interpreter: recorded failure '$marker')"
        else
          UNVERIFIED=$((UNVERIFIED + 1))
          unverified_list="$unverified_list\n  unverified $label: ${oracle#unverified:}"
          ok "$label: native exit $exit_want + stdout, UNVERIFIED golden (interpreter: recorded failure '$marker')"
        fi
      fi
      ;;
    *) no "$label: interp=$interp is not yes or fails:<marker>" ;;
  esac
done < <(awk '$1 == "@run"' "$RULES")

# --- 5. Open-bug reproducers still reproduce. ---
# Bugs the examples exposed (docs/bugs/108-120, 122), the exit 2i specification found
# (123-124) the semantics probes found (127-131) and bugs 136-137, pinned the way check_bug077_contained.sh
# pins 077: each reproducer must still show its recorded failure. When one stops (the
# bug was fixed), this leg fails until the bug doc, the audit_bug_corpus.sh entry and
# this table are updated together — a fix cannot leave the ledger reading OPEN.
# Modes: compile-fails:<text> (alternatives ` || `), compiles (the bug is that it is
# accepted), interp-fails:<text> (the binary runs; the interpreter rejects it),
# runs-exit:<status> (wrong code: the binary builds and exits with this wrong status),
# wrong-exit:<code> (silent wrong code: the binary builds and runs, and its exit is NOT the
# correct <code>; the wrong value itself is not pinned, since it may be an uninitialized read),
# interp-prints:<text> (silent wrong value in the interpreter: the binary builds, and --interp
# runs to completion and prints exactly this wrong <text>),
# signals (memory corruption: the binary builds and is killed by a signal; which signal is
# not pinned, since the C allocator's response to a corrupted pointer differs by platform;
# a status above 128 is read as a signal, so a signals reproducer must exit below 128
# whenever it runs to completion).
open_bug_reproducers() {
  local R="tests/regressions" num rel mode text out rc alt found
  while IFS='|' read -r num rel mode text; do
    [ -z "$num" ] && continue
    local f="$R/$rel" label="bug $num ($rel)"
    if [ ! -f "$f" ]; then no "$label: reproducer missing"; continue; fi
    if ! grep -q '^\*\*Status:\*\* OPEN' docs/bugs/"$num"_*.md 2>/dev/null; then
      no "$label: docs/bugs/${num}_*.md is missing or no longer OPEN — drop this row with the fix"
      continue
    fi
    local had_ll=0; [ -e "$f.ll" ] && had_ll=1
    out=$("$COMPILER" "$f" -o "$TMP/bugrepro_$num" 2>&1); rc=$?
    [ "$had_ll" = 0 ] && rm -f "$f.ll"
    case "$mode" in
      compile-fails)
        if [ "$rc" -eq 0 ]; then no "$label now compiles — fixed? update docs/bugs/$num, the corpus entry and this table"; continue; fi
        found=0
        while IFS= read -r alt; do
          [ -n "$alt" ] && grep -qF -- "$alt" <<< "$out" && found=1
        done < <(printf '%s\n' "$text" | awk '{n = split($0, a, " [|][|] "); for (i = 1; i <= n; i++) print a[i]}')
        if [ "$found" = 1 ]; then ok "$label still fails to compile ('$text')"; else no "$label fails differently than recorded ('$text'):"; printf '%s\n' "$out" | awk 'NR<=3'; fi
        ;;
      compiles)
        if [ "$rc" -eq 0 ]; then ok "$label is still accepted"; else no "$label is now rejected — fixed? update docs/bugs/$num, the corpus entry and this table"; fi
        ;;
      wrong-exit)
        if [ "$rc" -ne 0 ]; then no "$label is now rejected — fixed? update docs/bugs/$num, the corpus entry and this table"; continue; fi
        bounded_run -i /dev/null "$INTERP_TIMEOUT" -- "$TMP/bugrepro_$num" > /dev/null 2>&1; rc=$?
        # A timeout is INCOMPLETE; a launch failure or a signal is not the recorded wrong value.
        if bounded_is_incomplete "$rc"; then inc "$label timed out twice: no observation"
        elif [ "$rc" -ge 124 ]; then no "$label did not run to an exit (status $rc: launch failure or signal)"
        elif [ "$rc" -eq "$text" ]; then no "$label now exits $text, the correct value — fixed? update docs/bugs/$num, the corpus entry and this table"
        else ok "$label still exits $rc where $text is correct"; fi
        ;;
      runs-exit)
        # Wrong code: the binary builds and exits with the recorded WRONG status.
        if [ "$rc" -ne 0 ]; then no "$label no longer compiles natively:"; printf '%s\n' "$out" | awk 'NR<=3'; continue; fi
        bounded_run -i /dev/null "$RUN_TIMEOUT" -- "$TMP/bugrepro_$num" > /dev/null 2>&1; rc=$?
        if bounded_is_incomplete "$rc"; then inc "$label timed out twice: no observation"
        elif [ "$rc" = "$text" ]; then ok "$label still exits $text (wrong code)"
        else no "$label exits $rc, not the recorded wrong $text — fixed? update docs/bugs/$num, the corpus entry and this table"; fi
        ;;
      signals)
        if [ "$rc" -ne 0 ]; then no "$label is now rejected — fixed? update docs/bugs/$num, the corpus entry and this table"; continue; fi
        bounded_run -i /dev/null "$RUN_TIMEOUT" -- "$TMP/bugrepro_$num" > /dev/null 2>&1; rc=$?
        # timeout(1) reports 124 for a timeout (INCOMPLETE after the re-run alone) and 125 - 127
        # for a launch failure; a
        # child killed by signal N is reported as 128 + N. A normal exit above 128 would
        # read the same, which is why a signals reproducer must exit below 128 when it
        # completes.
        if [ "$rc" -gt 128 ]; then ok "$label is still killed by a signal (status $rc)"
        elif bounded_is_incomplete "$rc"; then inc "$label timed out twice: no observation"
        elif [ "$rc" -ge 124 ]; then no "$label did not run to completion or a signal (status $rc: launch failure)"
        else no "$label now exits $rc without a signal — fixed? update docs/bugs/$num, the corpus entry and this table"; fi
        ;;
      interp-prints)
        if [ "$rc" -ne 0 ]; then no "$label no longer compiles natively:"; printf '%s\n' "$out" | awk 'NR<=3'; continue; fi
        out=$(bounded_run -i /dev/null "$INTERP_TIMEOUT" -- "$COMPILER" "$f" --interp 2>&1); rc=$?
        if bounded_is_incomplete "$rc"; then inc "$label: the interpreter timed out twice: no observation"
        elif [ "$rc" -ne 0 ]; then no "$label: the interpreter now fails (status $rc) instead of printing the recorded wrong '$text':"; printf '%s\n' "$out" | awk 'NR<=3'
        elif [ "$out" = "$text" ]; then ok "$label: the interpreter still prints the wrong '$text'"
        else no "$label: the interpreter prints '$out', not the recorded wrong '$text' — fixed? update docs/bugs/$num, the corpus entry and this table"; fi
        ;;
      interp-fails)
        if [ "$rc" -ne 0 ]; then no "$label no longer compiles natively:"; printf '%s\n' "$out" | awk 'NR<=3'; continue; fi
        out=$(bounded_run -i /dev/null "$INTERP_TIMEOUT" -- "$COMPILER" "$f" --interp 2>&1); rc=$?
        if bounded_is_incomplete "$rc"; then inc "$label: the interpreter timed out twice: no observation"
        elif [ "$rc" -eq 0 ]; then no "$label: the interpreter now runs it — fixed? update docs/bugs/$num, the corpus entry and this table"
        elif grep -qF -- "$text" <<< "$out"; then ok "$label: the interpreter still fails ('$text')"
        else no "$label: the interpreter fails differently than recorded ('$text'):"; printf '%s\n' "$out" | awk 'NR<=3'; fi
        ;;
      *) no "$label: unknown mode '$mode'" ;;
    esac
  done <<'TABLE'
114|bug114_submodule_intrinsic_links_c_symbol/repro.con|compile-fails|Undefined symbols || undefined reference || undefined symbol: sizeof
122|bug122_builtin_enum_canonical_layout/repro.con|runs-exit|2
216|bug216_field_write_non_mut_binding/repro.con|compiles|
318|bug318_a_struct_literal_in_an_if_while_or_match_head_is/repro.con|runs-exit|42
319|bug319_a_call_that_re_lends_a_mut_binding_is_not_overla/repro.con|runs-exit|42
320|bug320_main_may_return_int_or_bool_the_native_exit_stat/repro.con|runs-exit|42
321|bug321_a_bare_variant_name_ignores_a_payload_and_a_stru/repro.con|runs-exit|42
322|bug322_trusted_code_compares_raw_pointers_with_and_the/repro.con|runs-exit|42
323|bug323_break_with_a_value_is_accepted_inside_a_while_or/repro.con|runs-exit|1
324|bug324_a_copy_type_or_an_array_satisfies_a_destroy_boun/repro.con|runs-exit|42
325|bug325_a_call_statement_whose_result_is_not_unit_discar/repro.con|runs-exit|42
326|bug326_a_field_access_opens_a_heap_or_heaparray_implici/repro.con|runs-exit|42
327|bug327_an_integer_or_float_literal_with_no_expected_typ/repro.con|runs-exit|42
328|bug328_a_method_taking_self_or_mut_self_is_called_on_an/repro.con|runs-exit|42
332|bug332_a_float_literal_of_type_f32_is_rounded_twice_thr/repro.con|runs-exit|0
333|bug333_an_extra_type_argument_to_a_body_less_generic_de/repro.con|compiles|
331|bug331_c_style_for_loops_are_accepted_and_the_counted_l/repro.con|runs-exit|42
TABLE
}
if [ -z "$ONLY" ]; then
  echo "=== open-bug reproducers ==="
  open_bug_reproducers
fi

# --- 6. Negative controls. ---
run_controls() {
  local unit="examples/snippets/fib.con" dir="$TMP/controls" real_rule
  mkdir -p "$dir"
  real_rule=$(grep -E "^@run $unit " "$MANIFEST" | awk 'NR==1')
  if [ -z "$real_rule" ]; then no "controls: $unit has no @run rule to mutate"; return; fi
  control() { # name, expected substring, manifest rule line(s) for the control unit, [unit]
    local name="$1" want="$2" rules="$3" cu="${4:-$unit}" m="$dir/$1.txt"
    { grep -v '^[[:space:]]*@' "$MANIFEST"; printf '%s\n' "$rules"; } > "$m"
    if EXAMPLE_MANIFEST="$m" EXAMPLE_OUTPUTS_ONLY="$cu" EXAMPLE_OUTPUTS_IS_CONTROL=1 bash "$0" > "$dir/$name.log" 2>&1; then
      no "control $name: the gate PASSED a rule it must reject"
    elif grep -qF -- "$want" "$dir/$name.log"; then
      echo "  ok   control $name fails as it must ($want)"; PASS=$((PASS + 1))
    else
      no "control $name failed, but not for the expected reason ('$want'):"
      grep FAIL "$dir/$name.log" | awk 'NR<=3'
    fi
  }
  control wrong_exit "native exit 55, expected 54" "@run $unit fib exit=54 ret=54 interp=yes oracle=interp stdout=empty"
  control wrong_interp_value "interpreter output differs" "@run $unit fib exit=55 ret=311 interp=yes oracle=interp stdout=empty"
  control wrong_stdout "native stdout differs" "@run examples/hmac_sha256/src/main.con hmac exit=0 ret=0 interp=yes oracle=interp stdout=empty" "examples/hmac_sha256/src/main.con"
  control interp_now_runs "promote the rule to interp=yes" "@run $unit fib exit=55 ret=55 interp=fails:undefined oracle=unverified:control stdout=empty"
  control missing_rule "has no @run or @norun rule" ""
  control inconsistent_ret "is inconsistent with exit=55" "@run $unit fib exit=55 ret=56 interp=yes oracle=interp stdout=empty"
  control norun_no_reason "has no reason=" "@norun $unit"
  # Oracles: an expected output counts only with an independent check.
  control no_oracle "declares no oracle=" "@run $unit fib exit=55 ret=55 interp=yes stdout=empty"
  control unknown_oracle "is not interp, spec:<pointer> or unverified:<reason>" "@run $unit fib exit=55 ret=55 interp=yes oracle=binary stdout=empty"
  control interp_oracle_without_interp "oracle=interp but interp=fails" "@run $unit fib exit=55 ret=55 interp=fails:undefined oracle=interp stdout=empty"
  control unverified_but_interp_runs "so declare oracle=interp" "@run $unit fib exit=55 ret=55 interp=yes oracle=unverified:control stdout=empty"
  control unverified_no_reason "oracle=unverified: needs a reason" "@run $unit fib exit=55 ret=55 interp=fails:undefined oracle=unverified: stdout=empty"
  control spec_missing_file "the derivation pointer does not resolve" "@run $unit fib exit=55 ret=55 interp=yes oracle=spec:docs/no_such_derivation.md stdout=empty"
  control spec_missing_heading "the derivation pointer does not resolve" "@run $unit fib exit=55 ret=55 interp=yes oracle=spec:docs/project/GOLDEN_ORACLES.md#no-such-heading stdout=empty"
  # Positive control: the real rule passes on its own.
  if EXAMPLE_OUTPUTS_ONLY="$unit" EXAMPLE_OUTPUTS_IS_CONTROL=1 bash "$0" > "$dir/positive.log" 2>&1; then
    echo "  ok   positive control: the real rule for $unit passes"; PASS=$((PASS + 1))
  else
    no "positive control: the real rule for $unit fails:"
    grep FAIL "$dir/positive.log" | awk 'NR<=3'
  fi
  # The timeout helper (lib/bounded.sh), in a subshell with its own outcome file and log so the
  # controls do not count toward this run's own recovered/incomplete tally.
  local bres
  bres=$(
    BOUNDED_PARENT="$dir"; source "$ROOT_DIR/scripts/tests/lib/bounded.sh"
    cd "$dir" || exit 1
    out=$(bounded_run -r 1 1 -- sh -c 'echo EXPECTED; sleep 5'); rc=$?
    { bounded_is_incomplete "$rc" && [ -z "$out" ]; } && echo hang_twice_incomplete_no_output
    out=$(bounded_run 5 -- sh -c 'echo own; exit 124'); rc=$?
    { [ "$rc" = 124 ] && ! bounded_is_incomplete "$rc" && [ "$out" = own ]; } && echo own_exit_124_not_incomplete
    rm -f marker
    out=$(bounded_run -r 5 1 -- sh -c 'if [ -e marker ]; then echo second; else : > marker; sleep 3; fi'); rc=$?
    { [ "$rc" = 0 ] && [ "$out" = second ] && grep -q '^recovered' "$BOUNDED_LOG"; } && echo recovered_recorded
    BOUNDED_KILL_AFTER=1 bounded_run -r 1 1 -- sh -c 'trap "" TERM; sleep 47.25' > /dev/null 2>&1; rc=$?
    sleep 0.5
    { bounded_is_incomplete "$rc" && ! pgrep -f "sleep 47.25" > /dev/null 2>&1; } && echo term_ignoring_child_killed
    # The leader dies on SIGTERM; its child ignores it and would outlive a leader-only watchdog.
    BOUNDED_KILL_AFTER=1 bounded_run -r 1 1 -- sh -c '(trap "" TERM; exec sleep 48.25) & wait' > /dev/null 2>&1; rc=$?
    sleep 0.5
    { bounded_is_incomplete "$rc" && ! pgrep -f "sleep 48.25" > /dev/null 2>&1; } && echo term_ignoring_orphan_killed
    # A command's own output cannot fake a timeout, nor a prefix hide one.
    out=$(bounded_run 5 -- sh -c 'echo "timeout: sending signal TERM to command x" >&2; echo fine' 2>/dev/null); rc=$?
    { [ "$rc" = 0 ] && [ "$out" = fine ] && ! bounded_is_incomplete "$rc"; } && echo stderr_cannot_fake_timeout
    out=$(bounded_run -r 1 1 -- sh -c 'printf prefix >&2; sleep 5' 2>&1); rc=$?
    { bounded_is_incomplete "$rc" && [ -z "$out" ]; } && echo stderr_cannot_hide_timeout
    # A runner that reports nothing is never a pass: no output, incomplete, logged.
    out=$(BOUNDED_EXEC=/nonexistent/bounded_exec.py bounded_run 5 -- sh -c 'echo leaked' 2>/dev/null); rc=$?
    { bounded_is_incomplete "$rc" && [ -z "$out" ] && grep -q '^runner-failed' "$BOUNDED_LOG"; } && echo runner_failure_not_a_pass
    # Budgets are decimal seconds; a fraction works and a malformed budget is a usage error.
    out=$(bounded_run 0.5 -- sh -c 'echo frac'); rc=$?
    out2=$(bounded_run abc -- sh -c 'echo never' 2>/dev/null); rc2=$?
    { [ "$rc" = 0 ] && [ "$out" = frac ] && [ "$rc2" = 2 ] && [ -z "$out2" ]; } && echo fractional_and_malformed_budgets
    # A cancelled runner kills the command's group.
    bounded_run 30 -- sleep 49.25 > /dev/null 2>&1 &
    bgpid=$!
    for _ in 1 2 3 4 5 6 7 8 9 10; do pgrep -f "bounded_exec.py.*sleep 49.25" > /dev/null 2>&1 && break; sleep 0.2; done
    pkill -TERM -f "bounded_exec.py.*sleep 49.25" > /dev/null 2>&1
    wait "$bgpid" 2>/dev/null; sleep 0.5
    ! pgrep -f "sleep 49.25" > /dev/null 2>&1 && echo cancelled_runner_kills_group
    # Last: bounded_gate_status removes this sourcing's state.
    bounded_watchdog 1
    out=$("${TO[@]}" sh -c 'echo EXPECTED; sleep 5' 2> "$dir/wd.err"); rc=$?
    { [ "$rc" = 124 ] && [ -z "$out" ] && grep -q '^INCOMPLETE' "$dir/wd.err" && ! bounded_gate_status > /dev/null; } && echo watchdog_incomplete
  )
  for c in hang_twice_incomplete_no_output own_exit_124_not_incomplete recovered_recorded watchdog_incomplete \
           term_ignoring_child_killed term_ignoring_orphan_killed stderr_cannot_fake_timeout stderr_cannot_hide_timeout \
           runner_failure_not_a_pass fractional_and_malformed_budgets cancelled_runner_kills_group; do
    if grep -qx "$c" <<< "$bres"; then echo "  ok   bounded control $c"; PASS=$((PASS + 1))
    else no "bounded control $c: lib/bounded.sh does not behave as specified"; fi
  done
  # The mutation drivers' "incomplete is not a kill" predicate (lib/incomplete.sh) sees every form an
  # incomplete result is printed in, ignores zero counts, and both drivers use it.
  local ires
  ires=$(
    source "$ROOT_DIR/scripts/tests/lib/incomplete.sh"
    for line in "  INCOMPLETE examples/x [y]: timed out" "INCOMPLETE t1: timed out twice" \
                "INCOMPLETE: 1 command(s) timed out twice" "  incomplete: 2 (timed out twice)"; do
      printf '%s\n' "$line" > "$dir/inc.log"; log_reports_incomplete "$dir/inc.log" || echo "missed: $line"
    done
    for line in "X: PASS=3 FAIL=0 INCOMPLETE=0" "PASS: 6  FAIL: 0  INCOMPLETE: 0  RECOVERED: 0" \
                "=== Example outputs: 1 passed, 0 failed, 0 incomplete" "incomplete=0"; do
      printf '%s\n' "$line" > "$dir/inc.log"; log_reports_incomplete "$dir/inc.log" && echo "false hit: $line"
    done
    for d in scripts/tests/check_gate_mutation_coverage.sh scripts/tests/test_mutation.sh; do
      grep -q 'log_reports_incomplete' "$ROOT_DIR/$d" || echo "unused in $d"
    done
  )
  if [ -z "$ires" ]; then echo "  ok   control: incomplete results are recognised (never a mutation kill) and zero counts are not"; PASS=$((PASS + 1))
  else no "control: lib/incomplete.sh: $(echo "$ires" | tr '\n' ';')"; fi
  # Positive control: a real unverified golden passes and is counted, not hidden.
  local uunit="examples/snippets/union.con"
  if ! grep -qE "^@run $uunit .*oracle=unverified:" "$MANIFEST"; then
    no "positive control: $uunit no longer has an unverified rule; point this control at one that does"
  elif EXAMPLE_OUTPUTS_ONLY="$uunit" EXAMPLE_OUTPUTS_IS_CONTROL=1 bash "$0" > "$dir/positive_unverified.log" 2>&1 \
       && grep -qF "1 unverified" "$dir/positive_unverified.log" && grep -qF "unverified $uunit" "$dir/positive_unverified.log"; then
    echo "  ok   positive control: the unverified rule for $uunit passes and is counted and listed"; PASS=$((PASS + 1))
  else
    no "positive control: the unverified rule for $uunit fails or is not counted:"
    grep -E 'FAIL|unverified' "$dir/positive_unverified.log" | awk 'NR<=3'
  fi
}
if [ "${EXAMPLE_OUTPUTS_IS_CONTROL:-0}" != 1 ]; then
  echo "=== negative controls ==="
  run_controls
fi

echo ""
recovered=$(grep -c '^recovered' "$BOUNDED_LOG" || true)
echo "=== Example outputs: $PASS passed, $FAIL failed, $INCOMPLETE incomplete (timed out twice; $recovered recovered on the re-run alone), $NORUN units not run (each with a reason) ==="
bounded_summary
echo "=== Golden oracles: $ORACLE_INTERP checked by the interpreter, $ORACLE_SPEC derived from a specification, $UNVERIFIED unverified (tracked gaps) ==="
[ "$UNVERIFIED" -gt 0 ] && echo -e "Unverified goldens (no independent check yet):$unverified_list"
if [ "$INCOMPLETE" -gt 0 ]; then
  echo -e "Incomplete (not passed, not failed; a qualifying run contains none):$incomplete_list"
fi
if [ "$FAIL" -gt 0 ]; then
  echo -e "Failures:$errors"
  exit 1
fi
[ "$INCOMPLETE" -eq 0 ] || exit 1
