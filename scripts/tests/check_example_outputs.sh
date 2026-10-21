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
#   @run <unit> <case> exit=<N> interp=<mode> [ret=<V>] [stdin=<file>]
#        [stdout=empty] [-- <args...>]
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
# interp=yes: `concrete <src> --interp` must exit 0 and print the expected stdout
# followed by the line `<V>` when ret=V is given (the interpreter prints main's
# return value; the binary exits with it modulo 256, so V mod 256 must equal N).
# Without ret=, main returns Unit and N must be 0. interp=yes cannot be combined
# with args or stdin: the interpreter takes neither.
# interp=fails:<marker>: the interpreter must FAIL with <marker> in its output.
# When it starts running the program, this gate fails until the rule is promoted
# to interp=yes — a disagreement is never silently tolerated.
#
# Coverage is fail-closed: a unit with no rule, a rule for an unknown unit, a
# unit with both @run and @norun, a duplicate case, and an expected/*.stdout file
# no rule uses all FAIL.
#
# Negative controls (temporary manifest copies, one small unit) run at the end:
# a wrong exit code, a wrong interpreter value, a wrong stdout, interp=fails on a
# program the interpreter runs, a missing rule, interp=yes with args and an
# inconsistent ret= must each FAIL; the real rule must pass.
#
# Environment (used by the controls): EXAMPLE_MANIFEST, EXAMPLE_OUTPUTS_ONLY
# (restrict coverage and runs to one unit), EXAMPLE_OUTPUTS_IS_CONTROL=1.
# EXAMPLE_VALGRIND=1 runs every native binary under valgrind memcheck (Linux CI):
# a memory error fails the run deterministically, where the bare binary may only
# crash sometimes (bug 121: conlog freed a corrupted pointer, SIGSEGV on one CI
# run in several, never on macOS; valgrind reported it on every run).

set -uo pipefail

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

PASS=0
FAIL=0
NORUN=0
errors=""
ok() { PASS=$((PASS + 1)); echo "  ok   $1"; }
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
  exit_want=""; ret=""; stdin_rel=""; stdout_empty=0; interp=""
  for kv in "$@"; do
    case "$kv" in
      exit=*) exit_want="${kv#exit=}" ;;
      ret=*) ret="${kv#ret=}" ;;
      stdin=*) stdin_rel="${kv#stdin=}" ;;
      stdout=empty) stdout_empty=1 ;;
      interp=*) interp="${kv#interp=}" ;;
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
  if [ -z "$ret" ] && [ "$exit_want" -ne 0 ] && [ "$interp" = yes ]; then
    no "$label: exit=$exit_want with interp=yes needs ret=<main's value> (only a Unit main exits 0 without one)"
    continue
  fi
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
  (cd "$work" && env -i PATH="$PATH" LC_ALL=C timeout "$RUN_TIMEOUT" ${VG[@]+"${VG[@]}"} "$bin" $args < "$in" > "$TMP/nat_$n.out" 2> "$TMP/nat_$n.err")
  rc=$?
  native_ok=1
  if [ "${#VG[@]}" -gt 0 ] && [ "$rc" -eq 98 ] && [ "$exit_want" -ne 98 ]; then
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
  isrc="$u"; idir="."
  case "$u" in */) isrc="src/main.con"; idir="${u%/}" ;; esac
  (cd "$idir" && env -i PATH="$PATH" HOME="${HOME:-/}" LC_ALL=C timeout "$INTERP_TIMEOUT" "$COMPILER" "$isrc" --interp < /dev/null > "$TMP/int_$n.out" 2> "$TMP/int_$n.err")
  irc=$?
  case "$interp" in
    yes)
      if [ -n "$args" ] || [ -n "$stdin_rel" ]; then
        no "$label: interp=yes with args or stdin — the interpreter takes neither; use interp=fails:<marker>"
        continue
      fi
      cp "$exp" "$TMP/int_$n.want"
      [ -n "$ret" ] && echo "$ret" >> "$TMP/int_$n.want"
      if [ "$irc" -ne 0 ]; then
        no "$label: interpreter exited $irc"
        cat "$TMP/int_$n.err" "$TMP/int_$n.out" | awk 'NR<=3'
      elif ! cmp -s "$TMP/int_$n.want" "$TMP/int_$n.out"; then
        no "$label: interpreter output differs (expected stdout + main's value)"
        diff "$TMP/int_$n.want" "$TMP/int_$n.out" | awk 'NR<=8'
      elif [ "$native_ok" = 1 ]; then
        ok "$label: native exit $exit_want + stdout, interpreter agrees"
      fi
      ;;
    fails:*)
      marker="${interp#fails:}"
      marker="${marker//\~/ }"   # '~' stands for a space (rules are whitespace-split)
      if [ -z "$marker" ]; then no "$label: interp=fails: needs a marker"; continue; fi
      if [ "$irc" -eq 0 ]; then
        no "$label: interp=fails:$marker but the interpreter now runs it (exit 0) — promote the rule to interp=yes"
      elif ! grep -qF -- "$marker" "$TMP/int_$n.out" "$TMP/int_$n.err"; then
        no "$label: the interpreter fails, but not with '$marker':"
        cat "$TMP/int_$n.err" "$TMP/int_$n.out" | awk 'NR<=3'
      elif [ "$native_ok" = 1 ]; then
        ok "$label: native exit $exit_want + stdout (interpreter: recorded failure '$marker')"
      fi
      ;;
    *) no "$label: interp=$interp is not yes or fails:<marker>" ;;
  esac
done < <(awk '$1 == "@run"' "$RULES")

# --- 5. Open-bug reproducers still reproduce. ---
# Bugs the examples exposed (docs/bugs/108-120, 122), the exit 2i specification found
# (123-124) and the semantics probes found (127-131), pinned the way check_bug077_contained.sh
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
        timeout "$INTERP_TIMEOUT" "$TMP/bugrepro_$num" > /dev/null 2>&1 < /dev/null; rc=$?
        # A timeout, a launch failure or a signal is not the recorded wrong value.
        if [ "$rc" -ge 124 ]; then no "$label did not run to an exit (status $rc: timeout, launch failure or signal)"
        elif [ "$rc" -eq "$text" ]; then no "$label now exits $text, the correct value — fixed? update docs/bugs/$num, the corpus entry and this table"
        else ok "$label still exits $rc where $text is correct"; fi
        ;;
      runs-exit)
        # Wrong code: the binary builds and exits with the recorded WRONG status.
        if [ "$rc" -ne 0 ]; then no "$label no longer compiles natively:"; printf '%s\n' "$out" | awk 'NR<=3'; continue; fi
        timeout "$RUN_TIMEOUT" "$TMP/bugrepro_$num" < /dev/null > /dev/null 2>&1; rc=$?
        if [ "$rc" = "$text" ]; then ok "$label still exits $text (wrong code)"
        else no "$label exits $rc, not the recorded wrong $text — fixed? update docs/bugs/$num, the corpus entry and this table"; fi
        ;;
      signals)
        if [ "$rc" -ne 0 ]; then no "$label is now rejected — fixed? update docs/bugs/$num, the corpus entry and this table"; continue; fi
        timeout "$RUN_TIMEOUT" "$TMP/bugrepro_$num" > /dev/null 2>&1 < /dev/null; rc=$?
        # timeout(1) reports 124 for a timeout and 125 - 127 for a launch failure; a
        # child killed by signal N is reported as 128 + N. A normal exit above 128 would
        # read the same, which is why a signals reproducer must exit below 128 when it
        # completes.
        if [ "$rc" -gt 128 ]; then ok "$label is still killed by a signal (status $rc)"
        elif [ "$rc" -ge 124 ]; then no "$label did not run to completion or a signal (status $rc: timeout or launch failure)"
        else no "$label now exits $rc without a signal — fixed? update docs/bugs/$num, the corpus entry and this table"; fi
        ;;
      interp-prints)
        if [ "$rc" -ne 0 ]; then no "$label no longer compiles natively:"; printf '%s\n' "$out" | awk 'NR<=3'; continue; fi
        out=$(timeout "$INTERP_TIMEOUT" "$COMPILER" "$f" --interp 2>&1 < /dev/null); rc=$?
        if [ "$rc" -ne 0 ]; then no "$label: the interpreter now fails (status $rc) instead of printing the recorded wrong '$text':"; printf '%s\n' "$out" | awk 'NR<=3'
        elif [ "$out" = "$text" ]; then ok "$label: the interpreter still prints the wrong '$text'"
        else no "$label: the interpreter prints '$out', not the recorded wrong '$text' — fixed? update docs/bugs/$num, the corpus entry and this table"; fi
        ;;
      interp-fails)
        if [ "$rc" -ne 0 ]; then no "$label no longer compiles natively:"; printf '%s\n' "$out" | awk 'NR<=3'; continue; fi
        out=$(timeout "$INTERP_TIMEOUT" "$COMPILER" "$f" --interp 2>&1 < /dev/null); rc=$?
        if [ "$rc" -eq 0 ]; then no "$label: the interpreter now runs it — fixed? update docs/bugs/$num, the corpus entry and this table"
        elif grep -qF -- "$text" <<< "$out"; then ok "$label: the interpreter still fails ('$text')"
        else no "$label: the interpreter fails differently than recorded ('$text'):"; printf '%s\n' "$out" | awk 'NR<=3'; fi
        ;;
      *) no "$label: unknown mode '$mode'" ;;
    esac
  done <<'TABLE'
108|bug108_bool_cast_trunc/repro.con|compile-fails|invalid cast opcode for cast from 'i1' to 'i32'
109|bug109_f32_literal_invalid_ir/repro.con|compile-fails|floating point constant invalid for type
110|bug110_array_value_store/repro.con|compile-fails|defined with type 'ptr' but expected '[2 x i64]'
110|bug110_array_value_store/repro_cast.con|compile-fails|defined with type 'ptr' but expected '[3 x i8]'
111|bug111_main_params_accepted/repro.con|compiles|
112|bug112_user_intrinsic_e0116/repro.con|compile-fails|(E0116) foreign binding 'sizeof' does not declare its effects
113|bug113_submodule_extern_effects_unchecked/repro.con|compiles|
114|bug114_submodule_intrinsic_links_c_symbol/repro.con|compile-fails|Undefined symbols || undefined reference
115|bug115_vec_get_duplicates_linear/repro.con|compiles|
116|bug116_vec_push_first_field_through_ref/repro.con|compile-fails|use of undefined value '@vec_push'
117|bug117_interp_module_const/repro.con|interp-fails|undefined variable 'K'
118|bug118_interp_nested_module_import/repro.con|interp-fails|undefined function 'f'
122|bug122_builtin_enum_canonical_layout/repro.con|runs-exit|2
124|bug124_duplicate_parameter_names/repro.con|compile-fails|redefinition of argument
127|bug127_struct_literal_field_order/repro.con|runs-exit|65
128|bug128_interp_int_to_float_identity/repro.con|interp-prints|3
129|bug129_user_fn_captures_libc_symbol/repro.con|signals|
130|bug130_int_as_bool_invalid_ir/repro.con|compile-fails|invalid cast opcode for cast from 'i64' to 'i1'
131|bug131_reference_parameter_repointed/repro.con|interp-fails|ref path base 'local' not in env
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
  control wrong_exit "native exit 55, expected 54" "@run $unit fib exit=54 ret=54 interp=yes stdout=empty"
  control wrong_interp_value "interpreter output differs" "@run $unit fib exit=55 ret=311 interp=yes stdout=empty"
  control wrong_stdout "native stdout differs" "@run examples/hmac_sha256/src/main.con hmac exit=0 ret=0 interp=yes stdout=empty" "examples/hmac_sha256/src/main.con"
  control interp_now_runs "promote the rule to interp=yes" "@run $unit fib exit=55 ret=55 interp=fails:undefined stdout=empty"
  control missing_rule "has no @run or @norun rule" ""
  control interp_with_args "interp=yes with args or stdin" "@run $unit fib exit=55 ret=55 interp=yes stdout=empty -- extra"
  control inconsistent_ret "is inconsistent with exit=55" "@run $unit fib exit=55 ret=56 interp=yes stdout=empty"
  control norun_no_reason "has no reason=" "@norun $unit"
  # Positive control: the real rule passes on its own.
  if EXAMPLE_OUTPUTS_ONLY="$unit" EXAMPLE_OUTPUTS_IS_CONTROL=1 bash "$0" > "$dir/positive.log" 2>&1; then
    echo "  ok   positive control: the real rule for $unit passes"; PASS=$((PASS + 1))
  else
    no "positive control: the real rule for $unit fails:"
    grep FAIL "$dir/positive.log" | awk 'NR<=3'
  fi
}
if [ "${EXAMPLE_OUTPUTS_IS_CONTROL:-0}" != 1 ]; then
  echo "=== negative controls ==="
  run_controls
fi

echo ""
echo "=== Example outputs: $PASS passed, $FAIL failed, $NORUN units not run (each with a reason) ==="
if [ "$FAIL" -gt 0 ]; then
  echo -e "Failures:$errors"
  exit 1
fi
