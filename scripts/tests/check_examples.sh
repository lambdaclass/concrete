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
#   expected-error  — standalone compile must FAIL (diagnostics example)
#   known-bug NNN M — a valid program a compiler bug blocks: compile must FAIL
#                     with output containing M (alternatives separated by ` || `),
#                     and docs/bugs/NNN_*.md must exist and not read FIXED. When
#                     the bug is fixed the compile succeeds and THIS GATE FAILS,
#                     so the entry is promoted instead of silently staying blocked.
#   skip owner=G reason=R
#                   — not compiled here because gate G (scripts/tests/*.sh) owns
#                     it. G must exist, be run by the CI workflow, and reference
#                     the file; R must be non-empty. A skip without a valid owner
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

# Skip-owner checks, in Python so they read whole files (no grep -q under pipefail)
# and compare whole TOKENS. A path token is a maximal run of [A-Za-z0-9_./-].
#   ref  <owner> <src>: the owner names <src> as a complete token, or, for a
#        .../src/main.con, the example directory as a complete token. A longer
#        name (<dir>.backup, main.con.disabled, another file under <dir>) or a
#        top-level directory plus a common word does not count.
#   runs <owner> <workflow>: the owner is invoked by a `run:` command of the
#        workflow — inline or block — as a complete token after shell comments
#        are stripped. A step name, a YAML comment or an inline `# ...` does not.
OWNER_PY='
import re, sys
TOK = re.compile(r"[A-Za-z0-9_./-]+")
def tokens(text):
    return set(t.rstrip(".") for t in TOK.findall(text)) | set(TOK.findall(text))
def strip_shell_comment(line):
    out, q = [], None
    for i, c in enumerate(line):
        if q:
            if c == q: q = None
        elif c in "\"'"'"'": q = c
        elif c == "#" and (i == 0 or line[i - 1] in " \t;|&("): break
        out.append(c)
    return "".join(out)
def run_commands(path):
    lines = open(path).read().split("\n")
    cmds, i = [], 0
    while i < len(lines):
        m = re.match(r"^(\s*)(?:-\s+)?run:\s*(.*)$", lines[i])
        if not m:
            i += 1; continue
        indent, rest = len(m.group(1)), m.group(2)
        if rest.strip() and rest.strip()[0] in "|>":
            i += 1
            while i < len(lines) and (not lines[i].strip() or len(lines[i]) - len(lines[i].lstrip()) > indent):
                cmds.append(lines[i]); i += 1
        else:
            cmds.append(rest); i += 1
    return [strip_shell_comment(c) for c in cmds]
mode, a, b = sys.argv[1], sys.argv[2], sys.argv[3]
if mode == "ref":
    toks = tokens(open(a).read())
    if b in toks: sys.exit(0)
    if b.endswith("/src/main.con"):
        d = b[: -len("/src/main.con")]
        if d in toks or d + "/" in toks: sys.exit(0)
    sys.exit(1)
if mode == "runs":
    for c in run_commands(b):
        t = tokens(c)
        if a in t or "./" + a in t: sys.exit(0)
    sys.exit(1)
sys.exit(2)
'
owner_references() { python3 -c "$OWNER_PY" ref "$1" "$2"; }
workflow_runs() { python3 -c "$OWNER_PY" runs "$1" "$WORKFLOW"; }

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
  if ! workflow_runs "$owner"; then
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
    compile|compile-lib|project-build|expected-error)
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
      if "$COMPILER" "$src" --emit-llvm > "$OUT_DIR/$out_name.ll" 2> "$OUT_DIR/$out_name.err" \
         && clang -c -x ir -Wno-override-module "$OUT_DIR/$out_name.ll" -o "$OUT_DIR/$out_name.o" > "$OUT_DIR/$out_name.cc.log" 2>&1; then
        ok
      else
        no "$src (expected to compile as a library: --emit-llvm + clang -c)"
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
  unit_control ref_backup_directory 1 python3 -c "$OWNER_PY" ref "$fake" "$real_src"
  printf 'run examples/known_holes/returned_ref_provenance_map/src/main.con.disabled\n' > "$fake"
  unit_control ref_disabled_file 1 python3 -c "$OWNER_PY" ref "$fake" "$real_src"
  printf 'run xexamples/known_holes/returned_ref_provenance_map\n' > "$fake"
  unit_control ref_prefixed_path 1 python3 -c "$OWNER_PY" ref "$fake" "$real_src"
  printf 'cd examples/known_holes/returned_ref_provenance_map && build\n' > "$fake"
  unit_control ref_exact_directory 0 python3 -c "$OWNER_PY" ref "$fake" "$real_src"
  unit_control ref_real_owner 0 python3 -c "$OWNER_PY" ref "$real_owner" "$real_src"
  printf 'jobs:\n  a:\n    steps:\n      - name: %s\n        run: true # %s\n' "$real_owner" "$real_owner" > "$fwf"
  unit_control runs_name_and_inline_comment 1 python3 -c "$OWNER_PY" runs "$real_owner" "$fwf"
  printf 'jobs:\n  a:\n    steps:\n      - run: |\n          # bash %s\n          echo %s.bak\n' "$real_owner" "$real_owner" > "$fwf"
  unit_control runs_block_comment_and_longer_name 1 python3 -c "$OWNER_PY" runs "$real_owner" "$fwf"
  printf 'jobs:\n  a:\n    steps:\n      - run: |\n          echo start\n          bash ./%s\n' "$real_owner" > "$fwf"
  unit_control runs_block_command 0 python3 -c "$OWNER_PY" runs "$real_owner" "$fwf"
  unit_control runs_real_workflow 0 python3 -c "$OWNER_PY" runs "$real_owner" "$WORKFLOW"

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
