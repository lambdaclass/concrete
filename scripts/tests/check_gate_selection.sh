#!/usr/bin/env bash
# IMPACT GATE SELECTION, REPLAYED AGAINST THE FAILURES IT EXISTS TO PREVENT (R-0513).
#
# scripts/tests/select_gates.sh picks the workflow gate commands a diff can break, and the pre-push
# hook runs what it picks. A selector is only worth its speed if it never drops a gate the diff
# breaks, and the cases that matter are ones the old area map already missed. On 2026-10-22 four
# failures were first seen in CI, about 1.5 h each:
#   - bug 129's symbol mangling (c3ae877a) broke check_stack_budget.sh (an extern spelled like an
#     intrinsic became E0118) and the float round-trip judge that check_concrete_fmt.sh runs;
#   - safety-eval2's heap rule (bug 144) broke check_linear_discard.sh;
#   - the symbols rebase broke the trust gate's bug-audit corpus (run_tests.sh --trust-gate).
# Each diff is a FIXED file list in tests/fixtures/gate_selection/, taken from the real commits
# named in its header. The lists are written out so the controls keep their meaning on a shallow
# CI checkout and after history is rewritten; when the commits are present, the lists are
# checked against them.
#
# Over-selection is safe and costs minutes; a selector that selects everything would pass every
# must-select control, so precision is pinned too (a script-only diff, a docs-only diff), and so are
# the fail-safe (an unmapped path selects everything) and the runner wiring (`--impact-files`
# resolves to exactly the selector's list).
set -uo pipefail
source "$(dirname "${BASH_SOURCE[0]}")/lib/gate_env.sh" || exit 2
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"

PASS=0; FAIL=0; SKIP=0
ok()   { echo "  ok   $1"; PASS=$((PASS+1)); }
no()   { echo "  FAIL $1"; FAIL=$((FAIL+1)); }
skip() { echo "  skip $1"; SKIP=$((SKIP+1)); }

FIX="tests/fixtures/gate_selection"
SELECT="scripts/tests/select_gates.sh"
for f in "$SELECT" scripts/tests/lib/select_gates.py "$FIX"; do
  [ -e "$f" ] || { echo "FATAL: $f missing" >&2; exit 2; }
done

TMPD="$(mktemp -d)" || { echo "FATAL: mktemp failed" >&2; exit 2; }
trap 'rm -rf "$TMPD"' EXIT

bash scripts/tests/run_ci_gates_local.sh --plan > "$TMPD/inventory" 2>"$TMPD/inventory.err" \
  || { echo "FATAL: the runner could not resolve the gate inventory:" >&2; cat "$TMPD/inventory.err" >&2; exit 2; }
NINV="$(grep -c . "$TMPD/inventory")"
[ "$NINV" -gt 100 ] || { echo "FATAL: gate inventory has only $NINV commands" >&2; exit 2; }

# select NAME -> $TMPD/NAME.sel (one command per line); a selector failure is a FAIL, not an empty list
select_fixture() {
  local name="$1" src="$2"
  if ! bash "$SELECT" --files "$src" > "$TMPD/$name.sel" 2>"$TMPD/$name.err"; then
    no "$name: the selector failed: $(head -c 300 "$TMPD/$name.err")"
    return 1
  fi
  return 0
}
# has NAME GATE-TEXT: some selected command runs GATE-TEXT (a script name, optionally with flags)
has() { grep -qE "(^|[ /])$(printf '%s' "$2" | sed 's/[.[\*^$]/\\&/g')( |$)" "$TMPD/$1.sel"; }
must() {
  local name="$1"; shift
  local g
  for g in "$@"; do
    if has "$name" "$g"; then ok "$name selects $g"
    else no "$name does NOT select $g, which this diff broke in CI"; fi
  done
}
count() { grep -c . "$TMPD/$1.sel"; }

echo "=== fixture fidelity: each list is its commits' diff (when history is present) ==="
for p in "$FIX"/*.paths; do
  base="$(sed -n 's/^# base //p' "$p")"; head="$(sed -n 's/^# head //p' "$p")"
  name="$(basename "$p" .paths)"
  if [ -z "$base" ] || [ -z "$head" ]; then no "$name: no '# base'/'# head' header"; continue; fi
  if git cat-file -e "$base^{commit}" 2>/dev/null && git cat-file -e "$head^{commit}" 2>/dev/null; then
    if diff <(git diff --name-only "$base" "$head") <(grep -v '^#' "$p") >/dev/null; then
      ok "$name matches git diff ${base:0:8}..${head:0:8}"
    else
      no "$name differs from git diff ${base:0:8}..${head:0:8}"
    fi
  else
    skip "$name: ${base:0:8}/${head:0:8} not in this clone (shallow checkout); the list is used as written"
  fi
done

echo "=== replay: the 2026-10-22 CI-only failures are selected ==="
select_fixture symbols "$FIX/symbols_c3ae877a.paths" \
  && must symbols check_stack_budget.sh check_concrete_fmt.sh
select_fixture stackleg "$FIX/stack_budget_leg_26c409f8.paths" \
  && must stackleg check_stack_budget.sh
select_fixture float "$FIX/float_judge_1ee7c267.paths" \
  && must float check_concrete_fmt.sh
select_fixture safety "$FIX/safety_eval2_aaa4d0c7.paths" \
  && must safety check_linear_discard.sh
select_fixture rebase "$FIX/symbols_rebase_9757540e_b376b4a2.paths" \
  && must rebase "run_tests.sh --trust-gate"
# The same trust-gate failure without the compiler rule's help: a bug record (docs/bugs, the corpus
# map, a regression) and no Concrete/ change. The audit corpus reads docs/bugs.
select_fixture record "$FIX/bug103_record_a7323992.paths" \
  && must record "run_tests.sh --trust-gate"

echo "=== precision: a selector that selects everything would pass the above ==="
if [ -f "$TMPD/stackleg.sel" ]; then
  n="$(count stackleg)"
  if [ "$n" -lt $((NINV / 5)) ]; then ok "a gate-script + fixture diff selects $n of $NINV"
  else no "a gate-script + fixture diff selects $n of $NINV (>= 20%): the selector does not discriminate"; fi
fi
if select_fixture docs "$FIX/docs_only_92861f4e.paths"; then
  n="$(count docs)"
  if [ "$n" -le 6 ]; then ok "the docs-only diff selects $n gate command(s)"
  else no "the docs-only diff selects $n gate commands (> 6)"; fi
  heavy="$(grep -E 'run_tests\.sh|check_examples\.sh|test_ssa\.sh|check_example_outputs\.sh' "$TMPD/docs.sel" || true)"
  if [ -z "$heavy" ]; then ok "the docs-only diff selects no corpus runner"
  else no "the docs-only diff selects a corpus runner: $heavy"; fi
  sed 's/^/         /' "$TMPD/docs.sel"
  # Bug 252: a docs-only diff can break a link or an anchor, and the link gate used to be selected
  # by nothing at all.
  must docs check_doc_links.sh
fi
# A Markdown file no gate names (a research note): the link gate is still selected, and selecting it
# maps no file, so the fail-safe below is untouched. Assembled at run time, like the unmapped path.
printf '%s/%s\n' research README.md > "$TMPD/research.paths"
if select_fixture research "$TMPD/research.paths"; then
  must research check_doc_links.sh
  n="$(count research)"
  # Four today: the link gate, the hole-status gate, and the fast surface gates (plain and --mutate),
  # which run the spec gate. Its S13 check reads docs/spec/absorb_inventory.tsv, and that file names
  # documentation files, so editing or deleting a note can change its verdict. The cap of 5 leaves one
  # command of slack; corpus runners are refused for this diff explicitly below.
  if [ "$n" -le 5 ]; then ok "a research-note diff selects $n gate command(s)"
  else no "a research-note diff selects $n gate commands (> 5)"; fi
  heavy="$(grep -E 'run_tests\.sh|check_examples\.sh|test_ssa\.sh|check_example_outputs\.sh' "$TMPD/research.sel" || true)"
  if [ -z "$heavy" ]; then ok "the research-note diff selects no corpus runner"
  else no "the research-note diff selects a corpus runner: $heavy"; fi
  sed 's/^/         /' "$TMPD/research.sel"
fi

echo "=== rules: compiler, shared library, fail-safe ==="
printf 'Concrete/Resolve/BuiltinSigs.lean\n' > "$TMPD/compiler.paths"
if select_fixture compiler "$TMPD/compiler.paths"; then
  must compiler run_tests.sh check_stack_budget.sh check_linear_discard.sh check_concrete_fmt.sh
  n="$(count compiler)"
  if [ "$n" -ge $((NINV * 4 / 5)) ]; then ok "a compiler module selects $n of $NINV (every compiler- or Lean-running gate)"
  else no "a compiler module selects only $n of $NINV"; fi
fi
printf 'scripts/tests/lib/fresh.sh\n' > "$TMPD/lib.paths"
if select_fixture lib "$TMPD/lib.paths"; then
  must lib check_stack_budget.sh check_linear_discard.sh
fi
# Assembled at run time: a path spelled out in this file would be "read" by this gate.
printf '%s/%s\n' zz_unmapped_dir new_input.bin > "$TMPD/unmapped.paths"
if select_fixture unmapped "$TMPD/unmapped.paths"; then
  n="$(count unmapped)"
  if [ "$n" -eq "$NINV" ]; then ok "a path no rule maps selects every gate ($n) — fail-safe"
  else no "a path no rule maps selects $n of $NINV; the fail-safe must select every gate"; fi
fi

printf '.github/workflows/lean_action_ci.yml\n' > "$TMPD/wf.paths"
if SELECT_GATES_BASE=none select_fixture wfnobase "$TMPD/wf.paths"; then
  n="$(count wfnobase)"
  if [ "$n" -eq "$NINV" ]; then ok "a workflow edit with no base to compare selects every gate ($n)"
  else no "a workflow edit with no base selects $n of $NINV"; fi
fi
if SELECT_GATES_BASE=HEAD select_fixture wfhead "$TMPD/wf.paths"; then
  n="$(count wfhead)"
  if [ "$n" -lt "$NINV" ]; then ok "a workflow edit compared with a base selects only what reads it or changed ($n)"
  else no "a workflow edit compared with HEAD still selects every gate"; fi
fi

echo "=== indirect inputs (each one an under-selection found in review) ==="
# A manifest names the example; the gate's text names neither the example nor its expected output.
printf 'examples/base64_cli/expected/encode.stdout\n' > "$TMPD/expected.paths"
select_fixture expected "$TMPD/expected.paths" && must expected check_example_outputs.sh
# check_campaign_summary_golden.sh runs a one-family campaign through the mutation driver, which
# launches its children through this helper: a driver invoked in a bounded mode still runs its helpers.
printf 'scripts/tests/lib/run_campaign_child.py\n' > "$TMPD/child.paths"
select_fixture child "$TMPD/child.paths" && must child check_campaign_summary_golden.sh
# A workflow edit that re-spells a command (new arguments) selects that command: compared against
# main before this gate was registered, the gate's own command is new.
if git cat-file -e "713778e2^{commit}" 2>/dev/null; then
  if SELECT_GATES_BASE=713778e2 select_fixture wfadd "$TMPD/wf.paths"; then
    must wfadd check_gate_selection.sh
  fi
else
  skip "713778e2 not in this clone: the added-command leg needs the base's workflow"
fi

echo "=== text is not a call: names in keyed-array values (bug 237) ==="
# A corpus note naming a gate script made run_tests.sh, which runs the corpus audit, look as if it ran
# that gate and read docs/: the docs-only control selected 19 commands in train 16. A keyed array's
# values are text only when every use of them is (shell_text.py); a value the script reads is a read,
# one it may run is a call. The gate named in the fixtures is the runner, which this gate already
# runs, so spelling it here adds no edge to this gate.
NAMED="scripts/tests/run_ci_gates_local.sh"
# script_edges FILE -> "run PATH" / "read PATH" lines: the selector's own reading of FILE
script_edges() {
  python3 -I - "$1" <<'PY'
import sys
sys.path.insert(0, "scripts/tests/lib")
import select_gates as sg
files, dirs, _, _, runs = sg.Repo([]).script(sys.argv[1])
for f in sorted(runs):
    print("run", f)
for f in sorted(files | dirs):
    print("read", f)
PY
}
# leg NAME EXPECT DESCRIPTION < FIXTURE. EXPECT: none (no edge to the runner nor to a docs path),
# read (a read and no run) or run.
leg() {
  local name="$1" want="$2" what="$3" got
  cat > "$TMPD/$name.fixture"
  if ! script_edges "$TMPD/$name.fixture" > "$TMPD/$name.edges" 2>"$TMPD/$name.edges.err"; then
    no "$what: the selector could not read the fixture: $(head -c 300 "$TMPD/$name.edges.err")"; return
  fi
  if grep -qx "run $NAMED" "$TMPD/$name.edges"; then got=run
  elif grep -qx "read $NAMED" "$TMPD/$name.edges" || grep -q "^read docs" "$TMPD/$name.edges"; then got=read
  else got=none; fi
  if [ "$got" = "$want" ]; then ok "$what ($got)"
  else no "$what: expected $want, got $got: $(tr '\n' ' ' < "$TMPD/$name.edges")"; fi
}
cat > "$TMPD/map_notes" <<'EOF'
declare -A NOTES=(
  # a note per bug, as in the corpus audit
  [901]="FIXED -- the regression vehicle is scripts/tests/run_ci_gates_local.sh itself (its --plan leg)"
  [902]='OPEN -- reproduced by: bash scripts/tests/run_ci_gates_local.sh --plan; see docs/bugs/'
)
NOTES[903]="FIXED -- pinned by run_ci_gates_local.sh"
for n in "${!NOTES[@]}"; do
  entry="${NOTES[$n]}"
  [[ -n "${NOTES[$n]:-}" && "$entry" != x ]] && echo "  skip $n (${NOTES[$n]})"
  word="$(printf '%s' "$entry" | awk '{print $1}')"
  case "$word" in FIXED*) : ;; esac
done
EOF
leg notes none "a gate named in a map's note text is neither run nor read, nor is the directory it mentions" < "$TMPD/map_notes"
# Through a file, not a pipe: a leg in a pipeline would count its result in a subshell.
{ cat "$TMPD/map_notes"; echo 'bash "$ROOT_DIR/scripts/tests/run_ci_gates_local.sh" --plan'; } > "$TMPD/map_and_call"
leg call run "a genuine invocation of the same gate in the same file is still a call" < "$TMPD/map_and_call"
leg exec run "a map value run through a variable is a call" <<'EOF'
declare -A GATE_FOR=(
  [plan]="scripts/tests/run_ci_gates_local.sh"
)
g="${GATE_FOR[plan]}"
bash "$g" --plan
EOF
leg cmdpos run "a map value in command position, behind a wrapper's options, is a call" <<'EOF'
declare -A GATE_FOR=( [plan]="scripts/tests/run_ci_gates_local.sh" [x]=other )
for k in plan; do nice -n 5 "${GATE_FOR[$k]}" --plan; done
EOF
leg chain run "a map value copied into another map that is run is a call" <<'EOF'
declare -A A=( [k]="scripts/tests/run_ci_gates_local.sh" )
declare -A B=( [k]="${A[k]}" )
run_gate "${B[k]}"
EOF
leg readval read "a map value the script reads as a file is a read, not a call" <<'EOF'
declare -A SRC=( [k]="scripts/tests/run_ci_gates_local.sh" )
grep -c . "${SRC[k]}"
EOF
leg key read "a map key naming a gate is a read, not a call" <<'EOF'
declare -A SKIP=(
  ["scripts/tests/run_ci_gates_local.sh"]="a reason that names nothing"
)
for f in scripts/tests/*.sh; do [ -n "${SKIP[$f]:-}" ] && echo "skip $f"; done
EOF
leg keysub run "a command substitution in a key runs at assignment" <<'EOF'
declare -A M=( [$(bash scripts/tests/run_ci_gates_local.sh --plan | head -1)]=note )
EOF
leg ansi run "an ANSI-C quoted value does not swallow the invocation after it" <<'EOF'
declare -A M=( [k]=$'it\'s' )
bash scripts/tests/run_ci_gates_local.sh --plan
echo ' ) '
EOF
leg quote run "a quote inside \${...} does not swallow the invocation after it" <<'EOF'
declare -A M=( [k]=${UNSET:-'}'} )
bash scripts/tests/run_ci_gates_local.sh --plan
echo ' ) '
EOF
leg printfv read "a value moved by printf -v and then read is a read" <<'EOF'
declare -A M=( [k]="scripts/tests/run_ci_gates_local.sh" )
printf -v path '%s' "${M[k]}"
wc -l "$path"
EOF
leg piped run "a value printed into a consumer the reader cannot see is a call" <<'EOF'
declare -A M=( [k]="scripts/tests/run_ci_gates_local.sh" )
printf '%s\n' "${M[k]}" |
  while read -r cmd; do "$cmd" --plan; done
EOF
leg nameref run "an array reached through a nameref is a call" <<'EOF'
declare -A M=( [k]="scripts/tests/run_ci_gates_local.sh" )
declare -n alias=M
"${alias[k]}" --plan
EOF
leg printfv2 run "a value moved by an attached printf -vNAME and then run is a call" <<'EOF'
declare -A M=( [k]="scripts/tests/run_ci_gates_local.sh" )
printf -vpath '%s' "${M[k]}"
"$path" --plan
EOF
leg indirect run "an indirect expansion inside another array's element makes every array a call" <<'EOF'
declare -A M=( [k]="scripts/tests/run_ci_gates_local.sh" )
ref='M[k]'
declare -A N=( [k]="${!ref}" )
"${N[k]}" --plan
EOF
leg filtered run "a value printed through a filter into a file is a call" <<'EOF'
declare -A M=( [k]="scripts/tests/run_ci_gates_local.sh" )
printf '%s\n' "${M[k]}" | sort > "$tmp"
. "$tmp"
EOF
leg group run "a value printed inside a group whose output goes to a file is a call" <<'EOF'
declare -A M=( [k]="scripts/tests/run_ci_gates_local.sh" )
{
  printf '%s\n' "${M[k]}"
} > "$tmp"
run_it "$tmp"
EOF
leg inlinegroup run "a value printed inside an inline group whose output goes to a file is a call" <<'EOF'
declare -A M=( [k]="scripts/tests/run_ci_gates_local.sh" )
{ printf '%s\n' "${M[k]}"; } > "$tmp"
run_it "$tmp"
EOF
leg procsub run "a value printed into a process substitution is a call" <<'EOF'
declare -A M=( [k]="scripts/tests/run_ci_gates_local.sh" )
mapfile -t gates < <(printf '%s\n' "${M[@]}" | sort)
for gate in "${gates[@]}"; do "$gate" --plan; done
EOF
leg procsub2 run "a value printed into a process substitution over several lines is a call" <<'EOF'
declare -A M=( [k]="scripts/tests/run_ci_gates_local.sh" )
mapfile -t gates < <(
  printf '%s\n' "${M[@]}" | sort
)
for gate in "${gates[@]}"; do "$gate" --plan; done
EOF
leg funcout run "a value printed by a function whose output is captured is a call" <<'EOF'
declare -A M=( [k]="scripts/tests/run_ci_gates_local.sh" )
get_gate() {
  printf '%s\n' "${M[$1]}"
}
gate=$(get_gate k)
"$gate" --plan
EOF
leg funcout2 run "the same, for a function declared with the function keyword" <<'EOF'
declare -A M=( [k]="scripts/tests/run_ci_gates_local.sh" )
function get_gate {
  printf '%s\n' "${M[$1]}"
}
gate=$(get_gate k)
"$gate" --plan
EOF
leg reply run "a value read into the implicit REPLY and run is a call" <<'EOF'
declare -A M=( [plan]="scripts/tests/run_ci_gates_local.sh" )
read -r <<< "${M[plan]}"
"$REPLY" --plan
EOF
leg shadow run "a function named like a whitelisted command makes the array a call" <<'EOF'
declare -A M=( [k]="scripts/tests/run_ci_gates_local.sh" )
cat() { "$@"; }
cat "${M[k]}"
EOF
leg bashenv run "a value assigned to a variable the shell reads by name is a call" <<'EOF'
declare -A M=( [k]="scripts/tests/run_ci_gates_local.sh" )
BASH_ENV="${M[k]}"
bash -c :
EOF
leg readquoted run "a value read into a destination the reader cannot name is a call" <<'EOF'
declare -A M=( [k]="scripts/tests/run_ci_gates_local.sh" )
read -r 'cmd' <<< "${M[k]}"
"$cmd" --plan
EOF
leg sorto run "a value piped into a filter that writes a file is a call" <<'EOF'
declare -A M=( [k]="scripts/tests/run_ci_gates_local.sh" )
printf '%s\n' "${M[k]}" | sort -o "$tmp"
. "$tmp"
EOF
leg awkvar run "a value piped into awk running a program held in a variable is a call" <<'EOF'
declare -A M=( [k]="scripts/tests/run_ci_gates_local.sh" )
program='{system($0)}'
printf '%s\n' "${M[k]}" | awk "$program"
EOF
leg getter run "a value printed by a function called through a variable is a call" <<'EOF'
declare -A M=( [k]="scripts/tests/run_ci_gates_local.sh" )
get_gate() { printf '%s\n' "${M[k]}"; }
getter=get_gate
cmd=$("$getter")
"$cmd"
EOF
leg rematch run "a value matched by =~ (captured into BASH_REMATCH) is a call" <<'EOF'
declare -A M=( [k]="scripts/tests/run_ci_gates_local.sh" )
[[ ${M[k]} =~ (.*) ]]
"${BASH_REMATCH[1]}"
EOF
leg testop read "a value tested by an operator held in a variable is a read" <<'EOF'
declare -A M=( [k]="scripts/tests/run_ci_gates_local.sh" )
op=-f
test "$op" "${M[k]}" && echo present
EOF
leg globdir read "a value a glob enumerates is a read" <<'EOF'
declare -A M=( [k]="docs/bugs" )
for f in "${M[k]}"/*; do :; done
EOF
leg catpipe run "a file read into a pipe continued on the next line is a call" <<'EOF'
declare -A M=( [k]="scripts/tests/run_ci_gates_local.sh" )
cat "${M[k]}" |
  bash
EOF
leg stdin run "a value redirected into a command that is not a reader is a call" <<'EOF'
declare -A M=( [k]="scripts/tests/run_ci_gates_local.sh" )
run_it < "${M[k]}"
EOF
leg catfile run "a file read into another file is a call (that file may be run)" <<'EOF'
declare -A M=( [k]="scripts/tests/run_ci_gates_local.sh" )
cat "${M[k]}" > "$tmp"
. "$tmp"
EOF
leg defassign run "a value assigned by \${var:=...} is a call" <<'EOF'
declare -A M=( [k]="scripts/tests/run_ci_gates_local.sh" )
: "${gate:=${M[k]}}"
"$gate" --plan
EOF
leg unquoted read "a value expanded unquoted (split and globbed) is a read" <<'EOF'
declare -A M=( [k]="docs/bugs/*.md" )
printf '%s\n' ${M[k]}
EOF
leg heredoc run "a value expanded into a heredoc is a call" <<'EOF'
declare -A M=( [k]="scripts/tests/run_ci_gates_local.sh" )
read -r gate <<END
${M[k]}
END
"$gate" --plan
EOF
leg inlinedef run "a function defined mid-line, named like a reader, is a call" <<'EOF'
declare -A M=( [k]="scripts/tests/run_ci_gates_local.sh" )
if true; then cat ( ) { "$@"; }; fi
cat "${M[k]}" --plan
EOF
leg fdfile run "a value printed by >&file into a file is a call" <<'EOF'
declare -A M=( [k]="scripts/tests/run_ci_gates_local.sh" )
printf '%s\n' "${M[k]}" >&"$tmp"
. "$tmp"
EOF
leg groupdup run "a group whose output is duplicated and redirected to a file is a call" <<'EOF'
declare -A M=( [k]="scripts/tests/run_ci_gates_local.sh" )
{
  printf '%s\n' "${M[k]}"
} 2>&1 >"$tmp"
. "$tmp"
EOF
leg execout run "a value printed after exec sends output to a file is a call" <<'EOF'
declare -A M=( [k]="scripts/tests/run_ci_gates_local.sh" )
exec &>"$tmp"
printf '%s\n' "${M[k]}"
EOF
leg valsub run "a command substitution in a value runs at assignment, however its line reads" <<'EOF'
declare -A M=(
  [k]="$(scripts/tests/run_ci_gates_local.sh --plan | head -n 1)"
)
EOF
leg rg run "a value handed to rg, which can run a preprocessor, is a call" <<'EOF'
declare -A M=( [k]="scripts/tests/run_ci_gates_local.sh" )
rg --pre "${M[k]}" pattern README.md
EOF
leg awk run "a value handed to awk, which can run commands, is a call" <<'EOF'
declare -A M=( [k]="scripts/tests/run_ci_gates_local.sh" )
awk -v cmd="${M[k]}" 'BEGIN { system(cmd " --plan") }'
EOF

echo "=== wiring: run_ci_gates_local.sh --impact-files resolves to exactly the selector's list ==="
grep -v '^#' "$FIX/stack_budget_leg_26c409f8.paths" > "$TMPD/wire.paths"
if bash scripts/tests/run_ci_gates_local.sh --plan --impact-files "$TMPD/wire.paths" > "$TMPD/wire.plan" 2>/dev/null; then
  if diff <(sort "$TMPD/wire.plan") <(sort "$TMPD/stackleg.sel") >/dev/null; then
    ok "the runner's impact plan equals the selector output ($(grep -c . "$TMPD/wire.plan") commands)"
  else
    no "the runner's impact plan differs from the selector output"
  fi
else
  no "run_ci_gates_local.sh --plan --impact-files failed"
fi

echo
echo "GATE-SELECTION: PASS=$PASS FAIL=$FAIL SKIP=$SKIP"
[ "$FAIL" -eq 0 ]
