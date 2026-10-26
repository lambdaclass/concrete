#!/usr/bin/env bash
# Example coverage gate.
#
# Drives scripts/tests/example_manifest.txt, the single source of truth for
# what every .con file under examples/ is expected to do:
#
#   compile         — standalone compile must succeed
#   compile-lib     — a module with no `main` (proof/VC teaching code): its LLVM IR
#                     must be emitted and compile to an object (`clang -c`)
#   project-build   — `concrete build` in the owning project dir must succeed
#   project-lib     — a project that cannot be linked in CI: `concrete build
#                     --emit-llvm` there must succeed and the IR compile to an object
#   expected-error  — standalone compile must FAIL (diagnostics example)
#   known-bug NNN M — a valid program a compiler bug blocks: compile must FAIL
#                     with output containing M (alternatives separated by ` || `),
#                     and docs/bugs/NNN_*.md must exist and not read FIXED. When
#                     the bug is fixed the compile succeeds and THIS GATE FAILS,
#                     so the entry is promoted instead of silently staying blocked.
#   skip owner=G reason=R
#                   — not compiled here because gate G (scripts/tests/*.sh) owns
#                     it. G must exist, be invoked by an inline `run:` command of
#                     the CI workflow, and reference the file by its full path (or
#                     its example directory) as a whole token; R must be non-empty. A skip without a valid owner
#                     FAILS: an example nobody checks is not an option.
#
# Lines starting with `@` are run rules for check_example_outputs.sh; this gate
# ignores them.
#
# Coverage rule: a .con file under examples/ that is not in the manifest is a
# FAILURE — new examples cannot silently go unchecked. A manifest entry whose
# file no longer exists is also a failure.
#
# Negative controls run at the end against temporary manifest copies (structure
# only, no compiles): an unowned skip, an owner that does not reference its file,
# an owner that does not exist, a missing reason, an unlisted .con, and a
# known-bug naming a fixed bug must each FAIL; the real manifest must pass.
#
# Compiled binaries are left in $EXAMPLES_OUT_DIR (default: a temp dir).
#
# Environment (used by the controls): EXAMPLE_MANIFEST (manifest path),
# EXAMPLES_STRUCTURE_ONLY=1 (validate the manifest without compiling),
# EXAMPLES_IS_CONTROL=1 (do not recurse into the controls), EXAMPLES_WORKFLOW
# (workflow path).

set -uo pipefail
source "$(dirname "${BASH_SOURCE[0]}")/lib/gate_env.sh" || exit 2

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"

COMPILER=".lake/build/bin/concrete"
MANIFEST="${EXAMPLE_MANIFEST:-scripts/tests/example_manifest.txt}"
WORKFLOW="${EXAMPLES_WORKFLOW:-.github/workflows/lean_action_ci.yml}"
STRUCTURE_ONLY="${EXAMPLES_STRUCTURE_ONLY:-0}"

if [ "$STRUCTURE_ONLY" != 1 ] && [ ! -x "$COMPILER" ]; then
  echo "error: compiler not found at $COMPILER. Run 'lake build' first." >&2
  exit 2
fi
if [ ! -f "$MANIFEST" ]; then
  echo "error: manifest not found at $MANIFEST" >&2
  exit 2
fi
if [ ! -f "$WORKFLOW" ]; then
  echo "error: CI workflow not found at $WORKFLOW (skip owners are checked against it)" >&2
  exit 2
fi

OUT_DIR="${EXAMPLES_OUT_DIR:-}"
TMP_ROOT=$(mktemp -d)
trap 'rm -rf "$TMP_ROOT"' EXIT
if [ -z "$OUT_DIR" ]; then
  OUT_DIR="$TMP_ROOT/out"
fi
mkdir -p "$OUT_DIR"

PASS=0
FAIL=0
SKIP=0
BLOCKED=0
errors=""

ok() { PASS=$((PASS + 1)); }
no() {
  FAIL=$((FAIL + 1))
  errors="$errors\n  FAIL $1"
  echo "FAIL: $1"
}

# The build-outcome lines: no comments, no blanks, no `@` run rules.
entries() { grep -v '^[[:space:]]*#' "$MANIFEST" | grep -v '^[[:space:]]*$' | grep -v '^[[:space:]]*@'; }

# --- 1. Coverage: every .con under examples/ is in the manifest, and ---
# --- every manifest path still exists on disk.                       ---
manifest_paths=$(entries | awk '{print $1}' | LC_ALL=C sort)
disk_paths=$(find examples -name '*.con' | LC_ALL=C sort)

# Collation is pinned to C for this comparison. `comm` assumes its inputs are sorted the way it
# compares them, and a UTF-8 collator ignores punctuation at the primary level — so an entry like
# `__concrete_check_oom` sorts differently under LANG=en_US.UTF-8 than under C, and the set
# difference comes out WRONG rather than merely reordered. That made one gate's verdict depend on
# the developer's locale; pinned here so it cannot.
unlisted=$(LC_ALL=C comm -23 <(echo "$disk_paths") <(echo "$manifest_paths"))
if [ -n "$unlisted" ]; then
  while IFS= read -r f; do
    no "$f is not listed in $MANIFEST (add it with its expected outcome)"
  done <<< "$unlisted"
else
  ok
fi

missing=$(LC_ALL=C comm -13 <(echo "$disk_paths") <(echo "$manifest_paths"))
if [ -n "$missing" ]; then
  while IFS= read -r f; do
    no "$f is listed in $MANIFEST but does not exist on disk"
  done <<< "$missing"
else
  ok
fi

dups=$(echo "$manifest_paths" | uniq -d)
if [ -n "$dups" ]; then
  while IFS= read -r f; do
    no "$f appears more than once in $MANIFEST"
  done <<< "$dups"
else
  ok
fi

# --- 2. Drive each manifest entry. project-build dirs are built once. ---
built_projects=""

# Find the nearest ancestor directory (within examples/) with a Concrete.toml.
project_root_of() {
  local dir
  dir=$(dirname "$1")
  while [ "$dir" != "." ] && [ "$dir" != "/" ]; do
    if [ -f "$dir/Concrete.toml" ]; then
      echo "$dir"
      return 0
    fi
    dir=$(dirname "$dir")
  done
  return 1
}

# Skip-owner checks (whole path tokens; workflow `run:` invocations, not mentions).
# See the helper's docstring for the exact rules.
OWNER_PY_FILE="$ROOT_DIR/scripts/tests/lib/example_skip_owner.py"
owner_references() { python3 "$OWNER_PY_FILE" ref "$1" "$2"; }
workflow_runs() { python3 "$OWNER_PY_FILE" runs "$1" "$WORKFLOW"; }

check_skip() { # src, rest-of-line
  local src="$1" rest="$2" owner reason
  owner=$(printf '%s\n' "$rest" | sed -n 's/^.*owner=\([^[:space:]]*\).*$/\1/p')
  reason=$(printf '%s\n' "$rest" | sed -n 's/^.*reason=//p' | sed 's/^[[:space:]]*//;s/[[:space:]]*$//')
  if [ -z "$owner" ]; then
    no "$src is 'skip' but has no owner (write 'skip owner=scripts/tests/<gate>.sh reason=<why>')"
    return
  fi
  if [ -z "$reason" ]; then
    no "$src is 'skip' with owner $owner but no reason= text"
    return
  fi
  case "$owner" in
    scripts/tests/*.sh) ;;
    *) no "$src: skip owner '$owner' is not a scripts/tests/*.sh gate"; return ;;
  esac
  if [ ! -f "$owner" ]; then
    no "$src: skip owner '$owner' does not exist"
    return
  fi
  case "$owner" in
    scripts/tests/check_examples.sh|scripts/tests/check_example_outputs.sh)
      no "$src: '$owner' cannot own a skip — it is the gate the skip exempts the file from"
      return ;;
  esac
  workflow_runs "$owner"
  local wr=$?
  if [ "$wr" -eq 2 ]; then
    no "$src: cannot read $WORKFLOW to check its skip owner (PyYAML missing? install python3-yaml)"
    return
  elif [ "$wr" -ne 0 ]; then
    no "$src: skip owner '$owner' is not run by $WORKFLOW (an owner CI never runs checks nothing)"
    return
  fi
  if ! owner_references "$owner" "$src"; then
    no "$src: skip owner '$owner' does not reference this file"
    return
  fi
  ok
  SKIP=$((SKIP + 1))
}

check_known_bug() { # src, number, marker
  local src="$1" num="$2" marker="$3" doc status out alt found
  case "$num" in
    [0-9][0-9][0-9]) ;;
    *) no "$src: known-bug needs a three-digit bug number, got '$num'"; return ;;
  esac
  if [ -z "$marker" ]; then
    no "$src: known-bug $num names no failure text to match"
    return
  fi
  doc=$(ls docs/bugs/"$num"_*.md 2>/dev/null | awk 'NR==1')
  if [ -z "$doc" ]; then
    no "$src: known-bug $num has no docs/bugs/${num}_*.md"
    return
  fi
  status=$(grep -m1 '^\*\*Status:\*\*' "$doc" | sed 's/^\*\*Status:\*\*[[:space:]]*//' | awk '{print toupper($1)}')
  case "$status" in
    OPEN*) ;;
    *) no "$src: known-bug $num is not an open bug ($doc reads '$status') — promote the example"; return ;;
  esac
  BLOCKED=$((BLOCKED + 1))
  [ "$STRUCTURE_ONLY" = 1 ] && { ok; return; }
  local had_ll=0
  [ -e "$src.ll" ] && had_ll=1
  out=$("$COMPILER" "$src" -o "$OUT_DIR/known_bug_should_not_exist" 2>&1)
  local rc=$?
  [ "$had_ll" = 0 ] && rm -f "$src.ll"   # a failed link leaves <file>.ll beside the source
  if [ "$rc" -eq 0 ]; then
    no "$src compiled — bug $num no longer blocks it: promote the entry (and update the bug's status)"
    return
  fi
  found=0
  while IFS= read -r alt; do
    alt=$(echo "$alt" | sed 's/^[[:space:]]*//;s/[[:space:]]*$//')
    [ -n "$alt" ] && grep -qF -- "$alt" <<< "$out" && found=1   # here-string: no pipe for -q to SIGPIPE
  done < <(printf '%s\n' "$marker" | awk '{n = split($0, a, " [|][|] "); for (i = 1; i <= n; i++) print a[i]}')
  if [ "$found" = 1 ]; then
    ok
  else
    no "$src (known-bug $num) failed differently than recorded; expected '$marker', got:"
    printf '%s\n' "$out" | awk 'NR<=5'
  fi
}

while read -r src outcome rest; do
  case "$outcome" in
    compile|compile-lib|project-build|project-lib|expected-error)
      if [ -n "${rest:-}" ]; then
        no "$src: '$outcome' takes no further fields (got '$rest')"
        continue
      fi
      ;;
  esac
  case "$outcome" in
    compile)
      [ "$STRUCTURE_ONLY" = 1 ] && { ok; continue; }
      out_name=$(echo "$src" | tr '/.' '__')
      if "$COMPILER" "$src" -o "$OUT_DIR/$out_name" > /dev/null 2>&1; then
        ok
      else
        no "$src (expected to compile)"
        "$COMPILER" "$src" -o "$OUT_DIR/$out_name" 2>&1 | awk 'NR<=5'
      fi
      ;;
    project-build)
      proj=$(project_root_of "$src") || { no "$src (project-build, but no Concrete.toml ancestor)"; continue; }
      case " $built_projects " in
        *" $proj "*) continue ;;
      esac
      built_projects="$built_projects $proj"
      [ "$STRUCTURE_ONLY" = 1 ] && { ok; continue; }
      if (cd "$proj" && "$ROOT_DIR/$COMPILER" build > /dev/null 2>&1); then
        ok
      else
        no "$proj (project expected to build)"
        (cd "$proj" && "$ROOT_DIR/$COMPILER" build 2>&1 | awk 'NR<=5')
      fi
      ;;
    compile-lib)
      [ "$STRUCTURE_ONLY" = 1 ] && { ok; continue; }
      # No `main` (a proof/VC teaching module): emit LLVM IR and compile it to an
      # object, which validates the IR exactly as linking a program would.
      out_name=$(echo "$src" | tr '/.' '__')
      had_ll=0; [ -e "$src.ll" ] && had_ll=1
      "$COMPILER" "$src" --emit-llvm > "$OUT_DIR/$out_name.ll" 2> "$OUT_DIR/$out_name.err"
      emit_rc=$?
      [ "$had_ll" = 0 ] && rm -f "$src.ll"   # --emit-llvm also writes <file>.ll beside the source
      if [ "$emit_rc" -eq 0 ] \
         && clang -c -x ir -Wno-override-module "$OUT_DIR/$out_name.ll" -o "$OUT_DIR/$out_name.o" > "$OUT_DIR/$out_name.cc.log" 2>&1; then
        ok
      else
        no "$src (expected to compile as a library: --emit-llvm + clang -c)"
        cat "$OUT_DIR/$out_name.err" "$OUT_DIR/$out_name.cc.log" 2>/dev/null | grep -v 'argument unused' | awk 'NR<=5'
      fi
      ;;
    project-lib)
      proj=$(project_root_of "$src") || { no "$src (project-lib, but no Concrete.toml ancestor)"; continue; }
      case " $built_projects " in
        *" $proj "*) continue ;;
      esac
      built_projects="$built_projects $proj"
      [ "$STRUCTURE_ONLY" = 1 ] && { ok; continue; }
      # A project that cannot be linked here (a native library is absent): emit its
      # LLVM IR and compile that to an object -- everything but the link.
      out_name=$(echo "$proj" | tr '/.' '__')
      before_ll=$(find "$proj" -name '*.con.ll' | LC_ALL=C sort)
      (cd "$proj" && "$ROOT_DIR/$COMPILER" build --emit-llvm > "$OUT_DIR/$out_name.ll" 2> "$OUT_DIR/$out_name.err")
      emit_rc=$?
      # --emit-llvm also writes <file>.ll beside each source; remove the ones it created.
      LC_ALL=C comm -13 <(echo "$before_ll") <(find "$proj" -name '*.con.ll' | LC_ALL=C sort) | while IFS= read -r ll; do
        [ -n "$ll" ] && rm -f "$ll"
      done
      if [ "$emit_rc" -eq 0 ] \
         && clang -c -x ir -Wno-override-module "$OUT_DIR/$out_name.ll" -o "$OUT_DIR/$out_name.o" > "$OUT_DIR/$out_name.cc.log" 2>&1; then
        ok
      else
        no "$proj (project expected to compile without linking: build --emit-llvm + clang -c)"
        cat "$OUT_DIR/$out_name.err" "$OUT_DIR/$out_name.cc.log" 2>/dev/null | grep -v 'argument unused' | awk 'NR<=5'
      fi
      ;;
    expected-error)
      [ "$STRUCTURE_ONLY" = 1 ] && { ok; continue; }
      if "$COMPILER" "$src" -o "$OUT_DIR/should_not_exist" > /dev/null 2>&1; then
        no "$src (expected a compile error, but it compiled)"
      else
        ok
      fi
      ;;
    known-bug)
      num=$(echo "${rest:-}" | awk '{print $1}')
      marker=$(echo "${rest:-}" | sed 's/^[[:space:]]*[^[:space:]]*[[:space:]]*//')
      check_known_bug "$src" "$num" "$marker"
      ;;
    skip)
      check_skip "$src" "${rest:-}"
      ;;
    *)
      no "$src has unknown outcome '$outcome' in $MANIFEST"
      ;;
  esac
done < <(entries)

# --- 3. Negative controls (structure only, temporary manifest copies). ---
run_controls() {
  local dir="$TMP_ROOT/controls" real="$MANIFEST"
  mkdir -p "$dir"
  local skip_line first_skip compile_line
  skip_line=$(entries | awk '$2=="skip"' | awk 'NR==1')
  first_skip=$(echo "$skip_line" | awk '{print $1}')
  compile_line=$(entries | awk '$2=="compile"' | awk 'NR==1')
  if [ -z "$skip_line" ] || [ -z "$compile_line" ]; then
    no "controls: the manifest has no skip or no compile entry to mutate — the controls would be vacuous"
    return
  fi
  # control <name> <expected-substring> <python-edit of s>; the edit must change the manifest.
  control() {
    local name="$1" want="$2" edit="$3" m="$dir/$1.txt" log="$dir/$1.log"
    MANIFEST_IN="$real" MANIFEST_OUT="$m" EDIT="$edit" python3 - <<'PY'
import os
s = open(os.environ['MANIFEST_IN']).read()
before = s
exec(os.environ['EDIT'])
assert s != before, 'control edit changed nothing'
open(os.environ['MANIFEST_OUT'], 'w').write(s)
PY
    if [ $? -ne 0 ]; then no "control $name: could not build its manifest"; return; fi
    if EXAMPLE_MANIFEST="$m" EXAMPLES_STRUCTURE_ONLY=1 EXAMPLES_IS_CONTROL=1 bash "$0" > "$log" 2>&1; then
      no "control $name: the gate PASSED a manifest it must reject"
    elif grep -qF -- "$want" "$log"; then
      echo "  ok   control $name fails as it must ($want)"
      ok
    else
      no "control $name failed, but not for the expected reason ('$want'):"
      awk 'NR<=5' "$log"
    fi
  }
  export SKIP_LINE="$skip_line" COMPILE_LINE="$compile_line"
  control unowned_skip "has no owner" \
    "import re, os; l = os.environ['SKIP_LINE']; assert l in s; s = s.replace(l, re.sub(r' owner=[^ ]*', '', l), 1)"
  control no_reason "but no reason= text" \
    "import re, os; l = os.environ['SKIP_LINE']; assert l in s; s = s.replace(l, re.sub(r' reason=.*', '', l), 1)"
  control owner_missing "does not exist" \
    "import re, os; l = os.environ['SKIP_LINE']; assert l in s; s = s.replace(l, re.sub(r'owner=[^ ]*', 'owner=scripts/tests/no_such_gate.sh', l), 1)"
  control owner_unrelated "does not reference this file" \
    "import re, os; l = os.environ['SKIP_LINE']; assert l in s; s = s.replace(l, re.sub(r'owner=[^ ]*', 'owner=scripts/tests/check_gate_hygiene.sh', l), 1)"
  control unlisted_con "is not listed in" \
    "import os; l = os.environ['COMPILE_LINE'] + '\n'; assert l in s; s = s.replace(l, '', 1)"
  control circular_owner "cannot own a skip" \
    "import re, os; l = os.environ['SKIP_LINE']; assert l in s; s = s.replace(l, re.sub(r'owner=[^ ]*', 'owner=scripts/tests/check_example_outputs.sh', l), 1)"
  # The review's bypass: a runnable example skipped to the outputs gate (which names
  # examples/snippets and contains the word `for`) and its @run deleted.
  control outputs_gate_owner "cannot own a skip" \
    "s = s.replace('examples/snippets/for.con compile', 'examples/snippets/for.con skip owner=scripts/tests/check_example_outputs.sh reason=covered', 1)"
  # An owner that names only a LONGER directory the example's is a prefix of.
  control prefix_directory "does not reference this file" \
    "import os; l = os.environ['SKIP_LINE']; p = l.split()[0]; assert p.endswith('_map/src/main.con'); s = s.replace(l, l.replace(p, p.replace('_map/src/main.con', '_ma/src/main.con')), 1); s += '\\n'"
  control known_bug_fixed "is not an open bug" \
    "import os; l = os.environ['COMPILE_LINE']; assert l in s; s = s.replace(l, l.split()[0] + ' known-bug 001 anything', 1)"
  control known_bug_no_doc "has no docs/bugs/" \
    "import os; l = os.environ['COMPILE_LINE']; assert l in s; s = s.replace(l, l.split()[0] + ' known-bug 999 anything', 1)"
  # Token-level controls on the owner checks (temporary owner / workflow files).
  local real_owner="scripts/tests/check_returned_ref_provenance.sh" fake="$dir/fake_owner.sh" fwf="$dir/fake_workflow.yml"
  local real_src="examples/known_holes/returned_ref_provenance_map/src/main.con"
  unit_control() { # name, expected-exit (0 pass / 1 fail), command...
    local name="$1" want="$2"; shift 2
    "$@"; local rc=$?
    if [ "$rc" = "$want" ]; then echo "  ok   control $name"; ok; else no "control $name: exit $rc, wanted $want"; fi
  }
  printf 'cd examples/known_holes/returned_ref_provenance_map.backup && build\n' > "$fake"
  unit_control ref_backup_directory 1 python3 "$OWNER_PY_FILE" ref "$fake" "$real_src"
  printf 'run examples/known_holes/returned_ref_provenance_map/src/main.con.disabled\n' > "$fake"
  unit_control ref_disabled_file 1 python3 "$OWNER_PY_FILE" ref "$fake" "$real_src"
  printf 'run xexamples/known_holes/returned_ref_provenance_map\n' > "$fake"
  unit_control ref_prefixed_path 1 python3 "$OWNER_PY_FILE" ref "$fake" "$real_src"
  printf 'cd examples/known_holes/returned_ref_provenance_map && build\n' > "$fake"
  unit_control ref_exact_directory 0 python3 "$OWNER_PY_FILE" ref "$fake" "$real_src"
  unit_control ref_real_owner 0 python3 "$OWNER_PY_FILE" ref "$real_owner" "$real_src"
  printf 'jobs:\n  a:\n    steps:\n      - name: %s\n        run: true # %s\n' "$real_owner" "$real_owner" > "$fwf"
  unit_control runs_name_and_inline_comment 1 python3 "$OWNER_PY_FILE" runs "$real_owner" "$fwf"
  printf 'jobs:\n  a:\n    steps:\n      - run: |\n          # bash %s\n          echo %s.bak\n' "$real_owner" "$real_owner" > "$fwf"
  unit_control runs_block_comment_and_longer_name 1 python3 "$OWNER_PY_FILE" runs "$real_owner" "$fwf"
  printf 'jobs:\n  a:\n    steps:\n      - run: |\n          echo start\n          bash ./%s\n' "$real_owner" > "$fwf"
  # Block scalars are not read at all (fail-closed): a genuine block invocation does
  # not count either, so an owner must be invoked by an inline run:.
  unit_control runs_block_not_read 1 python3 "$OWNER_PY_FILE" runs "$real_owner" "$fwf"
  printf 'jobs:\n  a:\n    steps:\n      - run: |\n          cat <<EOF\n          bash %s\n          EOF\n' "$real_owner" > "$fwf"
  unit_control runs_heredoc_body 1 python3 "$OWNER_PY_FILE" runs "$real_owner" "$fwf"
  printf 'jobs:\n  a:\n    steps:\n      - run: |\n          echo "first\n          bash %s"\n' "$real_owner" > "$fwf"
  unit_control runs_multiline_echo 1 python3 "$OWNER_PY_FILE" runs "$real_owner" "$fwf"
  unit_control runs_real_workflow 0 python3 "$OWNER_PY_FILE" runs "$real_owner" "$WORKFLOW"
  printf 'jobs:\n  a:\n    steps:\n      - run: |\n          echo harmless\n        name: %s\n' "$real_owner" > "$fwf"
  unit_control runs_name_after_block_run 1 python3 "$OWNER_PY_FILE" runs "$real_owner" "$fwf"
  printf 'jobs:\n  a:\n    steps:\n      - run: echo %s\n' "$real_owner" > "$fwf"
  unit_control runs_echo_argument 1 python3 "$OWNER_PY_FILE" runs "$real_owner" "$fwf"
  printf 'jobs:\n  a:\n    steps:\n      - run: printf "%%s" "bash %s"\n' "$real_owner" > "$fwf"
  unit_control runs_printf_quoted 1 python3 "$OWNER_PY_FILE" runs "$real_owner" "$fwf"
  printf 'jobs:\n  a:\n    steps:\n      - run: GATE=%s true\n' "$real_owner" > "$fwf"
  unit_control runs_assignment_value 1 python3 "$OWNER_PY_FILE" runs "$real_owner" "$fwf"
  printf 'jobs:\n  a:\n    steps:\n      - run: echo "check; bash %s; done"\n' "$real_owner" > "$fwf"
  unit_control runs_quoted_separator 1 python3 "$OWNER_PY_FILE" runs "$real_owner" "$fwf"
  printf 'jobs:\n  a:\n    steps:\n      - run: echo check\\; bash %s\n' "$real_owner" > "$fwf"
  unit_control runs_escaped_separator 1 python3 "$OWNER_PY_FILE" runs "$real_owner" "$fwf"
  printf 'jobs:\n  a:\n    steps:\n      - run: echo ";" bash %s\n' "$real_owner" > "$fwf"
  unit_control runs_quoted_standalone_separator 1 python3 "$OWNER_PY_FILE" runs "$real_owner" "$fwf"
  printf 'jobs:\n  a:\n    steps:\n      - run: echo \\; bash %s\n' "$real_owner" > "$fwf"
  unit_control runs_escaped_standalone_separator 1 python3 "$OWNER_PY_FILE" runs "$real_owner" "$fwf"
  # Compound commands are refused outright: their execution context is not modelled.
  printf 'jobs:\n  a:\n    steps:\n      - run: echo start; bash %s\n' "$real_owner" > "$fwf"
  unit_control runs_list_refused 1 python3 "$OWNER_PY_FILE" runs "$real_owner" "$fwf"
  printf 'jobs:\n  a:\n    steps:\n      - run: true || bash %s\n' "$real_owner" > "$fwf"
  unit_control runs_short_circuit_refused 1 python3 "$OWNER_PY_FILE" runs "$real_owner" "$fwf"
  printf 'jobs:\n  a:\n    steps:\n      - run: unused() (bash %s)\n' "$real_owner" > "$fwf"
  unit_control runs_function_body_refused 1 python3 "$OWNER_PY_FILE" runs "$real_owner" "$fwf"
  printf 'jobs:\n  a:\n    steps:\n      - run: |\n          cat <<EOF\n          run: bash %s\n          EOF\n' "$real_owner" > "$fwf"
  unit_control runs_key_inside_block 1 python3 "$OWNER_PY_FILE" runs "$real_owner" "$fwf"
  printf 'jobs:\n  a:\n    steps:\n      - run: bash -n %s\n' "$real_owner" > "$fwf"
  unit_control runs_syntax_check 1 python3 "$OWNER_PY_FILE" runs "$real_owner" "$fwf"
  printf 'jobs:\n  a:\n    steps:\n      - run: bash -en %s\n' "$real_owner" > "$fwf"
  unit_control runs_combined_noexec_flag 1 python3 "$OWNER_PY_FILE" runs "$real_owner" "$fwf"
  printf 'jobs:\n  a:\n    steps:\n      - run: bash -o noexec %s\n' "$real_owner" > "$fwf"
  unit_control runs_o_noexec 1 python3 "$OWNER_PY_FILE" runs "$real_owner" "$fwf"
  printf 'jobs:\n  a:\n    steps:\n      - run: bash -s %s\n' "$real_owner" > "$fwf"
  unit_control runs_bash_stdin 1 python3 "$OWNER_PY_FILE" runs "$real_owner" "$fwf"
  printf 'jobs:\n  a:\n    steps:\n      - run: sh -s %s\n' "$real_owner" > "$fwf"
  unit_control runs_sh_stdin 1 python3 "$OWNER_PY_FILE" runs "$real_owner" "$fwf"
  printf 'jobs:\n  a:\n    steps:\n      - run: bash -es %s\n' "$real_owner" > "$fwf"
  unit_control runs_combined_stdin_flag 1 python3 "$OWNER_PY_FILE" runs "$real_owner" "$fwf"
  printf 'jobs:\n  a:\n    steps:\n      - name: disabled\n        if: false\n        run: bash %s\n' "$real_owner" > "$fwf"
  unit_control runs_conditional_step 1 python3 "$OWNER_PY_FILE" runs "$real_owner" "$fwf"
  printf 'jobs:\n  a:\n    steps:\n      - run: bash %s\n        if: false\n' "$real_owner" > "$fwf"
  unit_control runs_conditional_step_after_run 1 python3 "$OWNER_PY_FILE" runs "$real_owner" "$fwf"
  printf 'jobs:\n  a:\n    if: false\n    runs-on: ubuntu-latest\n    steps:\n      - name: x\n        run: bash %s\n' "$real_owner" > "$fwf"
  unit_control runs_conditional_job 1 python3 "$OWNER_PY_FILE" runs "$real_owner" "$fwf"
  printf 'jobs:\n  a:\n    if: false\n    steps:\n      - run: echo a\n  b:\n    steps:\n      - name: x\n        run: bash %s\n' "$real_owner" > "$fwf"
  unit_control runs_unconditional_sibling_job 0 python3 "$OWNER_PY_FILE" runs "$real_owner" "$fwf"
  printf 'jobs:\n  a:\n    steps:\n      - run: bash %s\n        shell: bash --noprofile --norc -n {0}\n' "$real_owner" > "$fwf"
  unit_control runs_step_custom_shell 1 python3 "$OWNER_PY_FILE" runs "$real_owner" "$fwf"
  printf 'jobs:\n  a:\n    defaults:\n      run:\n        shell: bash -n {0}\n    steps:\n      - run: bash %s\n' "$real_owner" > "$fwf"
  unit_control runs_job_default_shell 1 python3 "$OWNER_PY_FILE" runs "$real_owner" "$fwf"
  printf 'defaults:\n  run:\n    shell: sh -n {0}\njobs:\n  a:\n    steps:\n      - run: bash %s\n' "$real_owner" > "$fwf"
  unit_control runs_workflow_default_shell 1 python3 "$OWNER_PY_FILE" runs "$real_owner" "$fwf"
  printf 'jobs:\n  a:\n    steps:\n      - run: bash %s\n        env:\n          SHELLOPTS: noexec\n' "$real_owner" > "$fwf"
  unit_control runs_step_env 1 python3 "$OWNER_PY_FILE" runs "$real_owner" "$fwf"
  printf 'jobs:\n  a:\n    env: { SHELLOPTS: noexec }\n    steps:\n      - run: bash %s\n' "$real_owner" > "$fwf"
  unit_control runs_job_env_flow 1 python3 "$OWNER_PY_FILE" runs "$real_owner" "$fwf"
  printf 'env: ${{ fromJSON(vars.E) }}\njobs:\n  a:\n    steps:\n      - run: bash %s\n' "$real_owner" > "$fwf"
  unit_control runs_workflow_env_expression 1 python3 "$OWNER_PY_FILE" runs "$real_owner" "$fwf"
  # Four-space job indentation: the job and its properties are derived, not assumed.
  printf 'jobs:\n    owner:\n        if: false\n        runs-on: ubuntu-latest\n        steps:\n          - run: bash %s\n' "$real_owner" > "$fwf"
  unit_control runs_wide_indent_conditional_job 1 python3 "$OWNER_PY_FILE" runs "$real_owner" "$fwf"
  printf 'jobs:\n    owner:\n        env:\n            SHELLOPTS: noexec\n        steps:\n          - run: bash %s\n' "$real_owner" > "$fwf"
  unit_control runs_wide_indent_job_env 1 python3 "$OWNER_PY_FILE" runs "$real_owner" "$fwf"
  printf 'jobs:\n    owner:\n        defaults:\n            run:\n                shell: bash -n {0}\n        steps:\n          - run: bash %s\n' "$real_owner" > "$fwf"
  unit_control runs_wide_indent_job_shell 1 python3 "$OWNER_PY_FILE" runs "$real_owner" "$fwf"
  printf 'jobs:\n    owner:\n        runs-on: ubuntu-latest\n        steps:\n          - run: bash %s\n' "$real_owner" > "$fwf"
  unit_control runs_wide_indent_plain_job 0 python3 "$OWNER_PY_FILE" runs "$real_owner" "$fwf"
  printf 'steps:\n  - run: bash %s\n' "$real_owner" > "$fwf"
  unit_control runs_no_job 1 python3 "$OWNER_PY_FILE" runs "$real_owner" "$fwf"
  printf 'jobs:\n  a:\n   # a comment at an odd column\n    if: false\n    steps:\n      - run: bash %s\n' "$real_owner" > "$fwf"
  unit_control runs_comment_hides_job_if 1 python3 "$OWNER_PY_FILE" runs "$real_owner" "$fwf"
  printf 'jobs:\n  a:\n    steps:\n      - run: bash %s < /missing-input\n' "$real_owner" > "$fwf"
  unit_control runs_input_redirection 1 python3 "$OWNER_PY_FILE" runs "$real_owner" "$fwf"
  printf 'jobs:\n  a:\n    steps:\n      - run: bash %s > /missing-dir/out\n' "$real_owner" > "$fwf"
  unit_control runs_output_redirection 1 python3 "$OWNER_PY_FILE" runs "$real_owner" "$fwf"
  printf 'jobs:\n  a:\n    steps:\n      - run: bash %s $(exit 1)\n' "$real_owner" > "$fwf"
  unit_control runs_command_substitution 1 python3 "$OWNER_PY_FILE" runs "$real_owner" "$fwf"
  printf 'jobs:\n  a:\n    steps:\n      - run: bash %s\n        shell: bash\n' "$real_owner" > "$fwf"
  # Steps and jobs are allowlisted: even a harmless key outside the list (shell: bash)
  # disqualifies, so an unexpected spelling of a harmful one cannot slip through.
  unit_control runs_unlisted_step_key 1 python3 "$OWNER_PY_FILE" runs "$real_owner" "$fwf"
  printf 'jobs:\n  a:\n    steps:\n      - name: x\n        if : false\n        run: bash %s\n' "$real_owner" > "$fwf"
  unit_control runs_spaced_if_key 1 python3 "$OWNER_PY_FILE" runs "$real_owner" "$fwf"
  printf 'jobs:\n  a:\n    steps:\n      - name: x\n        "if": false\n        run: bash %s\n' "$real_owner" > "$fwf"
  unit_control runs_quoted_if_key 1 python3 "$OWNER_PY_FILE" runs "$real_owner" "$fwf"
  printf 'jobs:\n  ok:\n    runs-on: ubuntu-latest\n    steps:\n      - run: echo fine\n  owner: # gate job\n    if: false\n    steps:\n      - run: bash %s\n' "$real_owner" > "$fwf"
  unit_control runs_commented_job_header 1 python3 "$OWNER_PY_FILE" runs "$real_owner" "$fwf"
  printf 'jobs:\n  a:\n    "if": false\n    steps:\n      - run: bash %s\n' "$real_owner" > "$fwf"
  unit_control runs_quoted_job_if 1 python3 "$OWNER_PY_FILE" runs "$real_owner" "$fwf"
  printf 'jobs:\n  disabled:\n    if: false\n    steps:\n      - run: echo x\n  owner:\n    needs: disabled\n    steps:\n      - run: bash %s\n' "$real_owner" > "$fwf"
  unit_control runs_needs_skipped_job 1 python3 "$OWNER_PY_FILE" runs "$real_owner" "$fwf"
  printf 'jobs:\n  disabled:\n    if: false\n    steps:\n      - run: echo x\n  mid:\n    needs: [disabled]\n    steps:\n      - run: echo y\n  owner:\n    needs: [mid]\n    steps:\n      - run: bash %s\n' "$real_owner" > "$fwf"
  unit_control runs_needs_transitively_skipped 1 python3 "$OWNER_PY_FILE" runs "$real_owner" "$fwf"
  printf 'jobs:\n  owner:\n    needs: missing\n    steps:\n      - run: bash %s\n' "$real_owner" > "$fwf"
  unit_control runs_needs_missing_job 1 python3 "$OWNER_PY_FILE" runs "$real_owner" "$fwf"
  printf 'jobs:\n  build:\n    runs-on: ubuntu-latest\n    steps:\n      - run: echo b\n  owner:\n    runs-on: ubuntu-latest\n    needs: build\n    steps:\n      - run: bash %s\n' "$real_owner" > "$fwf"
  unit_control runs_needs_unconditional_build 0 python3 "$OWNER_PY_FILE" runs "$real_owner" "$fwf"
  printf 'name: T\non: push\njobs:\n  owner:\n    name: "Display text\n      - run: bash %s\n      "\n    runs-on: ubuntu-latest\n    steps:\n      - run: echo harmless\n' "$real_owner" > "$fwf"
  unit_control runs_multiline_double_quoted 1 python3 "$OWNER_PY_FILE" runs "$real_owner" "$fwf"
  printf "name: T\non: push\njobs:\n  owner:\n    name: 'Display text\n      - run: bash %s\n      '\n    runs-on: ubuntu-latest\n    steps:\n      - run: echo harmless\n" "$real_owner" > "$fwf"
  unit_control runs_multiline_single_quoted 1 python3 "$OWNER_PY_FILE" runs "$real_owner" "$fwf"
  printf 'jobs:\n  a:\n    steps:\n      - run: "bash\\u00a0%s"\n' "$real_owner" > "$fwf"
  unit_control runs_nbsp_between_words 1 python3 "$OWNER_PY_FILE" runs "$real_owner" "$fwf"
  printf 'jobs:\n  a:\n    steps:\n      - run: "\\u00a0bash %s\\u00a0"\n' "$real_owner" > "$fwf"
  unit_control runs_nbsp_around_command 1 python3 "$OWNER_PY_FILE" runs "$real_owner" "$fwf"
  printf 'jobs:\n  a:\n    steps:\n      - run: "bash\\t%s"\n' "$real_owner" > "$fwf"
  unit_control runs_tab_separator 1 python3 "$OWNER_PY_FILE" runs "$real_owner" "$fwf"
  printf 'jobs:\n  a:\n    steps:\n      - run: python3 %s\n' "$real_owner" > "$fwf"
  unit_control runs_wrong_interpreter_python 1 python3 "$OWNER_PY_FILE" runs "$real_owner" "$fwf"
  printf 'jobs:\n  a:\n    steps:\n      - run: sh %s\n' "$real_owner" > "$fwf"
  unit_control runs_wrong_interpreter_sh 1 python3 "$OWNER_PY_FILE" runs "$real_owner" "$fwf"
  printf 'jobs:\n  a:\n    steps:\n      - run: ./%s\n' "$real_owner" > "$fwf"
  unit_control runs_direct_execution 1 python3 "$OWNER_PY_FILE" runs "$real_owner" "$fwf"
  printf 'jobs:\n  a:\n    steps:\n      - run: SHELLOPTS=noexec bash %s\n' "$real_owner" > "$fwf"
  unit_control runs_shellopts_noexec 1 python3 "$OWNER_PY_FILE" runs "$real_owner" "$fwf"
  printf 'jobs:\n  a:\n    steps:\n      - run: BASH_ENV=/dev/null bash %s\n' "$real_owner" > "$fwf"
  unit_control runs_bash_env 1 python3 "$OWNER_PY_FILE" runs "$real_owner" "$fwf"
  printf 'jobs:\n  a:\n    steps:\n      - run: bash -eu -o pipefail %s\n' "$real_owner" > "$fwf"
  unit_control runs_allowed_flags 0 python3 "$OWNER_PY_FILE" runs "$real_owner" "$fwf"
  printf 'jobs:\n  a:\n    steps:\n      - run: bash -c true %s\n' "$real_owner" > "$fwf"
  unit_control runs_command_string 1 python3 "$OWNER_PY_FILE" runs "$real_owner" "$fwf"
  printf 'jobs:\n  a:\n    steps:\n      - run: timeout --help bash %s\n' "$real_owner" > "$fwf"
  unit_control runs_timeout_wrapper 1 python3 "$OWNER_PY_FILE" runs "$real_owner" "$fwf"
  printf 'jobs:\n  a:\n    steps:\n      - run: env SHELLOPTS=noexec bash %s\n' "$real_owner" > "$fwf"
  unit_control runs_env_wrapper 1 python3 "$OWNER_PY_FILE" runs "$real_owner" "$fwf"
  printf 'jobs:\n  a:\n    steps:\n      - run: bash %s "$((1/0))"\n' "$real_owner" > "$fwf"
  unit_control runs_quoted_arithmetic 1 python3 "$OWNER_PY_FILE" runs "$real_owner" "$fwf"
  printf 'jobs:\n  a:\n    steps:\n      - run: bash %s "${UNSET:?}"\n' "$real_owner" > "$fwf"
  unit_control runs_quoted_unset_expansion 1 python3 "$OWNER_PY_FILE" runs "$real_owner" "$fwf"
  printf 'jobs:\n  a:\n    steps:\n      - run: X=1 bash %s\n' "$real_owner" > "$fwf"
  unit_control runs_any_assignment 1 python3 "$OWNER_PY_FILE" runs "$real_owner" "$fwf"
  printf 'jobs:\n  a:\n    steps:\n      - run: bash %s\n        working-directory: /tmp\n' "$real_owner" > "$fwf"
  unit_control runs_step_working_directory 1 python3 "$OWNER_PY_FILE" runs "$real_owner" "$fwf"
  printf 'jobs:\n  a:\n    defaults:\n      run:\n        working-directory: /tmp\n    steps:\n      - run: bash %s\n' "$real_owner" > "$fwf"
  unit_control runs_job_working_directory 1 python3 "$OWNER_PY_FILE" runs "$real_owner" "$fwf"
  printf 'defaults:\n  run:\n    working-directory: /tmp\njobs:\n  a:\n    steps:\n      - run: bash %s\n' "$real_owner" > "$fwf"
  unit_control runs_workflow_working_directory 1 python3 "$OWNER_PY_FILE" runs "$real_owner" "$fwf"
  printf 'defaults: # comment\n  run:\n    shell: bash -n {0}\njobs:\n  a:\n    steps:\n      - run: bash %s\n' "$real_owner" > "$fwf"
  unit_control runs_workflow_defaults_commented 1 python3 "$OWNER_PY_FILE" runs "$real_owner" "$fwf"
  printf "defaults: {run: {shell: 'bash -n {0}'}}\njobs:\n  a:\n    steps:\n      - run: bash %s\n" "$real_owner" > "$fwf"
  unit_control runs_workflow_defaults_flow 1 python3 "$OWNER_PY_FILE" runs "$real_owner" "$fwf"
  # The gate-shell default, exactly as the CI workflow spells it, is the one accepted; a near miss is not.
  printf 'defaults:\n  run:\n    shell: "nix develop .#gates --command bash --noprofile --norc -eo pipefail {0}"\njobs:\n  a:\n    steps:\n      - run: bash %s\n' "$real_owner" > "$fwf"
  unit_control runs_workflow_defaults_gates_shell 0 python3 "$OWNER_PY_FILE" runs "$real_owner" "$fwf"
  printf 'defaults:\n  run:\n    shell: "nix develop .#gates --command bash -n {0}"\njobs:\n  a:\n    steps:\n      - run: bash %s\n' "$real_owner" > "$fwf"
  unit_control runs_workflow_defaults_gates_shell_near_miss 1 python3 "$OWNER_PY_FILE" runs "$real_owner" "$fwf"
  printf 'defaults:\n  run:\n    shell: "nix develop .#gates --command bash --noprofile --norc -eo pipefail {0}"\n    working-directory: /tmp\njobs:\n  a:\n    steps:\n      - run: bash %s\n' "$real_owner" > "$fwf"
  unit_control runs_workflow_defaults_gates_shell_plus_wd 1 python3 "$OWNER_PY_FILE" runs "$real_owner" "$fwf"
  # A duplicate key: PyYAML keeps the last value, GitHub refuses the workflow.
  printf 'defaults:\n  run:\n    shell: "bash -n {0}"\n    shell: "nix develop .#gates --command bash --noprofile --norc -eo pipefail {0}"\njobs:\n  a:\n    steps:\n      - run: bash %s\n' "$real_owner" > "$fwf"
  unit_control runs_workflow_defaults_duplicate_key 1 python3 "$OWNER_PY_FILE" runs "$real_owner" "$fwf"
  printf 'jobs:\n  a:\n    defaults: # comment\n      run:\n        shell: bash -n {0}\n    steps:\n      - run: bash %s\n' "$real_owner" > "$fwf"
  unit_control runs_job_defaults_commented 1 python3 "$OWNER_PY_FILE" runs "$real_owner" "$fwf"
  printf "jobs:\n  a:\n    defaults: {run: {working-directory: /tmp}}\n    steps:\n      - run: bash %s\n" "$real_owner" > "$fwf"
  unit_control runs_job_defaults_flow 1 python3 "$OWNER_PY_FILE" runs "$real_owner" "$fwf"
  printf 'jobs:\n  a:\n    steps:\n      - run: bash %s\n          "${UNSET:?}"\n' "$real_owner" > "$fwf"
  unit_control runs_scalar_continuation 1 python3 "$OWNER_PY_FILE" runs "$real_owner" "$fwf"
  printf 'jobs:\n  a:\n    steps:\n      - run: bash -e ./%s\n' "$real_owner" > "$fwf"
  unit_control runs_prefixed_invocation 0 python3 "$OWNER_PY_FILE" runs "$real_owner" "$fwf"

  # The owner is mentioned in the workflow only inside a comment: it runs nothing.
  local owner wf="$dir/workflow_commented.yml"
  owner=$(echo "$skip_line" | sed -n 's/^.*owner=\([^[:space:]]*\).*$/\1/p')
  OWNER="$owner" WF_IN="$WORKFLOW" WF_OUT="$wf" python3 - <<'PY'
import os
o = os.environ['OWNER']
lines = open(os.environ['WF_IN']).read().split('\n')
out = [('# ' + l) if (o in l and not l.lstrip().startswith('#')) else l for l in lines]
assert out != lines, 'owner not in the workflow'
open(os.environ['WF_OUT'], 'w').write('\n'.join(out))
PY
  if EXAMPLES_WORKFLOW="$wf" EXAMPLES_STRUCTURE_ONLY=1 EXAMPLES_IS_CONTROL=1 bash "$0" > "$dir/wf.log" 2>&1; then
    no "control owner_only_in_comment: the gate PASSED an owner the workflow never runs"
  elif grep -qF "is not run by" "$dir/wf.log"; then
    echo "  ok   control owner_only_in_comment fails as it must (is not run by)"; ok
  else
    no "control owner_only_in_comment failed, but not for the expected reason:"; awk 'NR<=5' "$dir/wf.log"
  fi
  # Positive control: the real manifest's structure passes.
  if EXAMPLE_MANIFEST="$real" EXAMPLES_STRUCTURE_ONLY=1 EXAMPLES_IS_CONTROL=1 bash "$0" > "$dir/positive.log" 2>&1; then
    echo "  ok   positive control: the real manifest's structure passes"
    ok
  else
    no "positive control: the real manifest's structure is rejected:"
    awk 'NR<=5' "$dir/positive.log"
  fi
}

if [ "${EXAMPLES_IS_CONTROL:-0}" != 1 ]; then
  echo "=== negative controls (temporary manifest copies) ==="
  run_controls
fi

echo ""
echo "=== Example coverage: $PASS passed, $FAIL failed, $SKIP skipped (each with a valid owner), $BLOCKED blocked by an open bug ==="
if [ "$FAIL" -gt 0 ]; then
  echo -e "Failures:$errors"
  exit 1
fi
