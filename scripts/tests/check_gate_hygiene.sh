#!/usr/bin/env bash
# Gate-hygiene gate (ROADMAP #34b enforcement).
#
# "Fail means fail" must be uniform across the whole gate corpus. Twice (2026-06
# and 2026-07) CI stayed green while a gate was silently masking failures — a
# `cmd | tail` swallowing a nonzero exit, or a fail-counter that never turned
# into a nonzero exit code. `scripts/tests/lib/gate.sh` fixed the ergonomics;
# this gate LOCKS IN the invariant so a new hand-rolled gate can't regress it.
#
# Every shell gate (check_*.sh / test_*.sh) must:
#   1. be pipe-safe   — source lib/gate.sh, OR set `pipefail` itself; and
#   2. propagate fail — contain at least one failure-exit construct
#                       (`exit N`, a trailing `[ "$FAIL" -eq 0 ]`, `gate_finish`,
#                       `|| exit`, `return 1`, …) so a detected failure becomes a
#                       nonzero process exit.
#
# It is a source-structure gate (no compiler build needed) and runs in the
# `grammar` CI job next to the workflow-YAML gate.

set -euo pipefail
source "$(dirname "${BASH_SOURCE[0]}")/lib/gate_env.sh" || exit 2
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
GATES_DIR="$ROOT_DIR/scripts/tests"
cd "$GATES_DIR"

PASS=0
FAIL=0
ok() { echo "  ok   $1"; PASS=$((PASS+1)); }
no() { echo "  FAIL $1"; FAIL=$((FAIL+1)); }

# Gates that legitimately don't fit the shell-gate shape (delegate hygiene, or
# are helpers/libraries rather than standalone gates). Keep this list SHORT and
# justified — it is the audited escape hatch, not a dumping ground.
declare -A EXEMPT=(
  ["lib/gate.sh"]="shared harness, not a standalone gate"
)

echo "=== every shell gate is pipe-safe and propagates failure ==="
shopt -s nullglob
# _gate_shape <file>: prints `ok`, `not-pipe-safe` or `no-exit`. The environment guard's own line
# (`source .../lib/gate_env.sh || exit 2`, in every gate) is removed first: its `|| exit` says nothing
# about whether the gate's ASSERTIONS can fail, and left in, it made every gate pass this check.
_gate_shape() {
  local body pipe_safe=false propagates=false
  body="$(grep -vF 'lib/gate_env.sh" || exit 2' "$1" || true)"
  grep -qF "lib/gate.sh" <<<"$body" && pipe_safe=true
  grep -qE "set -[a-zA-Z]*o?[[:space:]]+pipefail|pipefail" <<<"$body" && pipe_safe=true

  # sourcing gate.sh brings gate_finish; otherwise require an explicit construct
  if grep -qF "lib/gate.sh" <<<"$body"; then
    propagates=true
  elif grep -qE "exit [1-9]|gate_finish|return 1|-eq 0[[:space:]]*\]|-gt 0|-ne 0|\|\|[[:space:]]*exit" <<<"$body"; then
    propagates=true
  # A GATE THAT `exec`s ANOTHER GATE HAS THE STRONGEST PROPAGATION THERE IS. `exec` REPLACES this
  # process, so the delegate's exit status is this script's exit status with nothing in between to
  # lose it — there is no `exit 1` to find because there is no shell left to run one. Thin wrappers
  # that run one scoped case of an expensive gate, so a single refusal can be mutation-covered
  # without paying for the full suite, were being reported as unable to fail.
  elif grep -qE '^[[:space:]]*exec ([a-z]+ )*(env [A-Za-z_]+=[^ ]+ )*bash "?\$' <<<"$body"; then
    propagates=true
  fi
  if $pipe_safe && $propagates; then echo ok
  elif ! $pipe_safe; then echo not-pipe-safe
  else echo no-exit; fi
}
for f in check_*.sh test_*.sh; do
  [[ -n "${EXEMPT[$f]:-}" ]] && { echo "  skip $f (${EXEMPT[$f]})"; continue; }
  case "$(_gate_shape "$f")" in
    ok) ok "$f" ;;
    not-pipe-safe) no "$f — not pipe-safe (add 'set -euo pipefail' or source lib/gate.sh)" ;;
    *) no "$f — no failure-exit construct (a detected failure won't set a nonzero exit code)" ;;
  esac
done
# CONTROL: a guarded gate whose assertions can never fail is still flagged — the guard's `|| exit 2`
# must not count as the gate's failure exit.
SHTMP="$(mktemp -d)"
printf '#!/usr/bin/env bash\nset -uo pipefail\nsource "$(dirname "${BASH_SOURCE[0]}")/lib/gate_env.sh" || exit 2\nFAIL=0\necho "FAIL=$FAIL"\n' > "$SHTMP/swallow.sh"
[ "$(_gate_shape "$SHTMP/swallow.sh")" = no-exit ] \
  && ok "CONTROL: a guarded gate with no failure exit of its own is flagged" \
  || no "CONTROL: the guard's '|| exit 2' passes for a gate that cannot fail"
rm -rf "$SHTMP"

echo ""
echo "=== no '| head -N' inside an errexit+pipefail script (SIGPIPE self-abort) ==="
# `cmd | head -N` closes the pipe once N lines arrive. A producer still writing
# then dies of SIGPIPE, and in a script with BOTH `set -e` and `pipefail` that
# failure becomes the script's exit status — so the gate fails for a reason
# unrelated to what it tests, and only when the producer loses the race against
# head's exit. That is exactly how the trust gate died on the R-0001 push: the
# bug-corpus audit capped 55 entries at thirty lines this way, green on one
# commit and exit-2 on the next. `awk "NR<=N"` reads all input and caps
# identically. (This comment avoids spelling the pattern, which the scan below
# would otherwise match in its own source.)
#
# Scoped to errexit scripts because that is where the SIGPIPE is fatal; gates
# using `set -uo pipefail` (no -e) absorb it. If a script here gains `set -e`,
# this check starts covering it.
hazard=0
for f in "$ROOT_DIR"/scripts/tests/*.sh; do
  head -20 "$f" | has_match -E "set -[a-z]*e[a-z]* .*pipefail|set -o errexit" || continue
  # `[h]ead` so this scan does not match its own pattern literal.
  hits="$(grep -nE "\| *[h]ead -[0-9]" "$f" || true)"
  if [ -n "$hits" ]; then
    no "$(basename "$f") — '| head -N' under errexit+pipefail: $(head -1 <<<"$hits" | cut -d: -f1). Use '| awk \"NR<=N\"'"
    hazard=1
  fi
done
[ "$hazard" -eq 0 ] && ok "no errexit gate script caps output with '| head -N'"

echo ""
echo "=== no 'local' statement reads a variable it assigns in the same statement ==="
# A builtin's arguments are word-expanded BEFORE the builtin runs, so in
#     local name="$1" f="$TMP/$name.con"
# `$name` is NOT the local being declared on the same line — it resolves against
# the enclosing scope. Two ways that goes wrong, and both happened at once in
# check_trap_inventory.sh:
#   * on CI, where the outer name is unset, `set -u` aborts the whole gate, so
#     it never ran at all;
#   * in the nix devshell, which EXPORTS `name=nix-shell-env`, it silently
#     expanded to that instead — the gate reported PASS=12 FAIL=0 while writing
#     every fixture to one wrong path.
# The second is the dangerous one: green for the wrong reason. The sibling gates
# spell the same line with `$1`, which is always set, which is why this read as
# idiomatic rather than as a hazard.
selfref=0
for f in "$ROOT_DIR"/scripts/tests/*.sh; do
  while IFS=: read -r lineno line; do
    [ -z "${lineno:-}" ] && continue
    body="${line#*local }"
    assigned=""
    bad=""
    # Walk the assignments left to right. A value referencing an ALREADY-listed
    # name on this same line is the hazard.
    for word in $body; do
      case "$word" in
        *=*)
          nm="${word%%=*}"
          val="${word#*=}"
          for prior in $assigned; do
            case "$val" in
              *"\$$prior"*|*"\${$prior}"*|*"\${$prior:"*) bad="$prior" ;;
            esac
          done
          case "$nm" in [A-Za-z_]*) assigned="$assigned $nm" ;; esac
          ;;
      esac
    done
    if [ -n "$bad" ]; then
      no "$(basename "$f"):$lineno — 'local' reads \$$bad, which it assigns in the same statement; split the declaration"
      selfref=1
    fi
  done < <(grep -nE '^[[:space:]]*local[[:space:]]+[A-Za-z_][A-Za-z0-9_]*=' "$f" || true)
done
[ "$selfref" -eq 0 ] && ok "no 'local' statement depends on its own earlier assignment"
echo ""
echo "=== concurrency guards are at the point of danger (docs/project/AGENTS.md, concurrent work) ==="
# Two agents shared one worktree on 2026-07-31 and the mutation harness restored a
# backup OVER a concurrent edit, then verified against the pre-edit hash and
# reported success — work destroyed, with the tool saying nothing was wrong. These
# legs keep the guards that close that from being quietly removed.

# 1. The harness must compare against what IT wrote, not only against the
#    original. Without the applied-hash it cannot tell its own mutation from a
#    third party's edit, which is precisely how the overwrite went unnoticed.
if grep -q "MUT_HASH_APPLIED" "$ROOT_DIR/scripts/tests/test_mutation.sh"; then
  ok "the mutation harness records the content it wrote"
else
  no "the mutation harness no longer records its own applied content — it cannot detect a foreign edit"
fi

# 2. It must REFUSE, not warn. A warning still leaves the file overwritten.
if grep -q "REFUSED to restore" "$ROOT_DIR/scripts/tests/test_mutation.sh" \
   && grep -q "CONCURRENT-EDIT" "$ROOT_DIR/scripts/tests/test_mutation.sh"; then
  ok "a foreign edit is refused and preserved, not overwritten"
else
  no "the mutation harness does not refuse/preserve a foreign edit"
fi

# 3. The push lock must live in the COMMON git dir. A per-worktree lock would let
#    two worktrees publish at once, and remotes are shared.
if grep -q "git-common-dir" "$ROOT_DIR/scripts/push-both.sh"; then
  ok "the publish lock is repo-wide (common git dir), not per-worktree"
else
  no "push-both's lock is not in the common git dir — two worktrees could publish at once"
fi

# 4. The one-command default must exist and be executable, or the rule is advice.
if [ -x "$ROOT_DIR/scripts/worktree-new.sh" ]; then
  ok "scripts/worktree-new.sh exists and is executable"
else
  no "scripts/worktree-new.sh is missing or not executable — isolation is not the cheap default"
fi

echo ""
echo "=== publication names a recorded SHA, never a moving ref ==="
# MEASURED: push-both mirrored `HEAD` while its CI wait had polled the SHA
# recorded at start. A commit made during the ~45min wait moved HEAD, so the
# mirror received a commit the primary did not have and CI never validated — the
# mirror ended up AHEAD of the primary, the one thing that script exists to
# prevent. Everything it checks is about the recorded SHA, so everything it
# pushes must be too.
if grep -nE 'git push .*"(HEAD)?:?\$BRANCH"' "$ROOT_DIR/scripts/push-both.sh" | has_match 'HEAD:'; then
  no "push-both pushes HEAD — a ref that can move between the CI check and the push"
else
  ok "push-both pushes only the recorded SHA, not HEAD"
fi
# Both pushes, primary and mirror, must name it.
# No COMPILED OUTPUT may be tracked. tests/programs/bug_064_aliased_imported_type
# (a 35KB Mach-O) reached origin because a broad `git add -A` swept the compiler's
# output in beside its .con source. .gitignore carries 16 one-off lines for exactly
# this, which is the fact-restated-where-it-can-drift shape: every new fixture needs
# a new line, and forgetting one is silent. gitignore globs cannot express "has no
# extension", and a glob that guessed wrong would silently hide a real fixture — the
# worse failure — so enforcement lives here instead.
#
# Scoped to EXTENSIONLESS files under tests/programs/, which is what the compiler
# emits beside foo.con. A first attempt at "no binary anywhere under tests/" failed
# on tests/fixtures/*.bin — those are deliberate ELF inputs for the ELF-parser
# tests, i.e. program data, not build output. Binary-ness alone does not identify
# an artifact; being an unextensioned sibling of a source program does.
artifacts=""
while IFS= read -r f; do
  [ -n "$f" ] || continue
  case "${f##*/}" in *.*) continue ;; esac
  artifacts="$artifacts $f"
done <<< "$(cd "$ROOT_DIR" && git ls-files 'tests/programs/*' 2>/dev/null)"
if [ -z "$artifacts" ]; then
  ok "no compiled output is tracked under tests/programs/"
else
  no "compiled outputs are tracked under tests/programs/:$artifacts"
fi

# The CI query must be scoped to the PUSH event. `--commit` alone also matches
# schedule-triggered runs, and a nightly run pending on the same tip made a wait
# time out while the push run had already succeeded.
# An empty run list must not read as a verdict. `gh run list --commit` requires the
# FULL 40-char SHA: given an abbreviation it returns [] rather than erroring, which
# is indistinguishable from "CI never ran".
if grep -q 'no CI run found' "$ROOT_DIR/scripts/push-both.sh"; then
  ok "the CI wait fails closed when no run matches the SHA"
else
  no "push-both does not handle an empty run list; an unresolvable SHA would read as a verdict"
fi

if grep -q 'gh run list .*--event push' "$ROOT_DIR/scripts/push-both.sh"; then
  ok "the CI wait filters to push-triggered runs"
else
  no "push-both's CI query is not scoped to --event push; a scheduled run on the same SHA can mask the real verdict"
fi

# The primary must be re-checked after the CI wait, not only before it: the wait is
# ~45min and the primary is shared, so it can advance in that window.
if grep -q 'pnow=' "$ROOT_DIR/scripts/push-both.sh" \
   && grep -q 'has advanced to' "$ROOT_DIR/scripts/push-both.sh"; then
  ok "the primary is re-verified after the CI wait, before mirroring"
else
  no "push-both mirrors without re-checking that the primary still holds the validated SHA"
fi
npush="$(grep -c 'git push .*"\$LOCAL:\$BRANCH"' "$ROOT_DIR/scripts/push-both.sh" || true)"
[ "$npush" = "2" ] \
  && ok "both the primary and mirror push name the recorded SHA" \
  || no "expected 2 pushes naming \$LOCAL, found $npush"

echo ""
echo "=== no conflict markers are committed ==="
# `git add -A` after a FAILED merge stages the conflicted file markers and all,
# and the next commit records them. That happened here: a ROADMAP.md merge
# conflicted, `git add -A` swept it up, and three markers landed in a merge
# commit. Nothing else would have caught it — the file still parses as Markdown.
markers="$(git -C "$ROOT_DIR" grep -lE '^(<<<<<<< |>>>>>>> |\|{7}( |$)|={7}$)' -- . 2>/dev/null \
  | grep -v 'check_gate_hygiene.sh' || true)"
if [ -z "$markers" ]; then
  ok "no tracked file contains merge conflict markers"
else
  no "conflict markers committed in: $(tr '\n' ' ' <<<"$markers")"
fi

echo ""
echo "=== no fixture source is silently ignored ==="
# The tests/regressions ignore rule once hid every single-project fixture's
# src/main.con (bug 080's model-diff fixture passed locally and could not exist on
# CI), while its comment promised new fixtures are never silently ignored. Both a
# synthetic path for each fixture layout and the real tree are checked.
for probe in tests/regressions/zz_probe/src/main.con tests/regressions/zz_probe/app/src/main.con \
             tests/regressions/zz_probe/repro.con tests/programs/zz_probe/main.con; do
  if git -C "$ROOT_DIR" check-ignore -q --no-index "$probe"; then
    no "fixture layout $probe would be ignored"
  else
    ok "fixture layout $probe is tracked"
  fi
done
ignored_src="$(git -C "$ROOT_DIR" ls-files --others --ignored --exclude-standard -- tests examples 2>/dev/null \
  | grep -E '\.(con|toml|lean|sh|py|json|md)$' | grep -v '/\.lake/' || true)"
if [ -z "$ignored_src" ]; then
  ok "no source-like file under tests/ or examples/ is ignored"
else
  no "ignored source-like files: $(tr '\n' ' ' <<<"$ignored_src")"
fi

echo ""
echo "=== the pre-push hook is installed in this clone ==="
# Advisory, not a failure: core.hooksPath is per-clone local config and cannot be
# versioned, so a gate cannot assert it for anyone else. It CAN tell the person
# running gates right now that their next push is unguarded — which is the moment
# the information is useful. The ritual lived only in a Makefile comment until
# 2026-07-25, and a comment did not stop two red pushes.
hp="$(git -C "$ROOT_DIR" config core.hooksPath 2>/dev/null || true)"
if [ "$hp" = ".githooks" ]; then
  ok "core.hooksPath=.githooks — pre-push runs the CI gate set"
else
  echo "  warn pre-push hook NOT installed in this clone — run 'make setup-hooks'"
  echo "       (advisory: local config, cannot be enforced from a versioned gate)"
fi

# ===========================================================================
# ADDED 2026-08-20, from the check_clean_checkout incident: a gate reported
# `PASS=11 FAIL=0` and exited 1 for two days, with 17 of its 37 assertions unreachable. A
# stray `exit 1` sat immediately before an orphan `fi`; bash executes incrementally, so the
# exit ran before the parser ever reached the invalid token. The syntax error was CONCEALED
# BY the statement that truncated the gate, and a human reading the summary and a runner
# reading `$?` believed different things about the same run.
#
# The controls above are about a gate MASKING a failure. These three are about a gate
# never reaching its assertions, or contradicting its own report.

# `find` with explicit -name filters, not `ls` with several globs: a glob argument that resolves to
# a DIRECTORY makes `ls` list its contents, so `__pycache__`, `lib/` and a pile of .py files reached
# `bash -n` and were reported as gates that do not parse. The predicate is what selects gates here,
# not the shape of the argument list.
# ABSOLUTE paths: this gate `cd`s into scripts/tests near the top, so a relative `find scripts/tests`
# searches scripts/tests/scripts/tests and quietly returns nothing.
mapfile -t HGATES < <(find "$ROOT_DIR/scripts/tests" "$ROOT_DIR/scripts/ci" -maxdepth 1 -type f \( -name 'check_*.sh' -o -name 'run_*.sh' \) 2>/dev/null | LC_ALL=C sort -u)

# NON-VACUITY FLOOR. The three controls below all iterate this list, so an empty one makes every one
# of them pass while checking nothing — and it did: after the path bug above, this reported
# "all 0 gates parse" as an OK. A control that cannot distinguish a clean corpus from an absent one
# is the exact defect this file exists to catch, so it is checked rather than assumed.
if [ "${#HGATES[@]}" -ge 100 ]; then
  ok "enumerated ${#HGATES[@]} shell gates for the parse/exit controls (floor 100)"
else
  no "only ${#HGATES[@]} gates enumerated — the three controls below would be vacuous"
fi

echo ""
echo "=== every gate parses under bash -n ==="
# Corpus-wide, not just files someone edited. The incident file was syntactically invalid
# on main the whole time; `bash -n` was run religiously that week on files edited by
# script, and never on the one edited by hand.
BADPARSE=""
for g in "${HGATES[@]}"; do bash -n "$g" 2>/dev/null || BADPARSE="$BADPARSE $(basename "$g")"; done
if [ -z "$BADPARSE" ]; then
  ok "all ${#HGATES[@]} gates parse"
else
  no "gate(s) do not parse:$BADPARSE"
fi

echo ""
echo "=== no unconditional early exit strands later assertions ==="
# Column zero only, so the many legitimate early exits inside conditionals are not flagged:
# a gate bailing on a precondition does so inside an `if`, while one exiting unconditionally
# mid-file is debugging residue with dead code beneath it.
STRANDED=""
for g in "${HGATES[@]}"; do
  # `awk NR<=1` rather than `head -1`: under errexit+pipefail a closing `head` can SIGPIPE the
  # producer and abort the gate. This file's own control above forbids it, and caught this line.
  eline="$(grep -nE '^(exit [0-9]+|GATE_DONE=1; echo [^;]*; exit [0-9]+)$' "$g" | awk 'NR<=1' | cut -d: -f1 || true)"
  [ -z "$eline" ] && continue
  etot="$(wc -l < "$g")"
  [ "$eline" -ge "$((etot - 2))" ] && continue
  eaft="$(awk -v n="$eline" 'NR>n' "$g" | grep -cE '^[[:space:]]*(ok|no) "' || true)"
  [ "${eaft:-0}" -gt 0 ] && STRANDED="$STRANDED $(basename "$g"):line$eline(+${eaft})"
done
if [ -z "$STRANDED" ]; then
  ok "no gate has an unconditional top-level exit with assertions after it"
else
  no "unconditional early exit strands assertions:$STRANDED"
fi

echo ""
echo "=== git grep: no engine-dependent pattern, no status read through a short-circuiting grep ==="
# Two hazards, one leg each, found together (CI run 39753399178, mutation shard 18):
#  1. `git grep -E/-G` compiles its pattern with the regex library git was built with, and the
#     boundary escapes `\b`, `\<`, `\>` are extensions: BSD/macOS git matches nothing with `\b`, a
#     GNU-regex git (Linux) treats it as a word boundary. A gate's verdict must not depend on which
#     git it found.
#  2. `git grep ... | grep -q` under `pipefail` reads git grep's SIGPIPE: when git grep has more to
#     write after the first match, `grep -q` exits, git grep dies, and the pipeline fails. A leg
#     meant to FAIL on a match then passes on whether the race was won.
# check_identity_separation.sh's C6 leg had both: `PackageIdentity\b` (live on Linux, where it also
# matched `noPackageIdentity` and a history comment) piped into `grep -q .`. It passed in CI's
# regular jobs by losing the race and went RED ON CLEAN in that shard by winning it.
# Scanned as LOGICAL lines: full-line comments are dropped first (a comment ending in a backslash
# continues nothing), lines ending in a backslash, `|` or `&&` are joined to the next, and heredoc
# bodies are skipped (they are data). A logical line running git grep (with any -C/-c options,
# quoted or not) is flagged for (1) if \b, \< or \> appears anywhere after `git ... grep`: inside
# quotes an alternation's `|` hides where the pattern ends, so keep a later plain grep on its own
# line, and a literal `git grep -F` pattern holding one is flagged too. (2) needs a pipe into
# `grep ... -q` with no `;` in between, so a capture followed by a separate test is not flagged.
# Known limits: patterns passed through a variable, and a trailing backslash inside single quotes.
logical_lines() { # files... -> FILE:LINE:joined logical line
  awk 'BEGIN { q = sprintf("%c", 39); hdre = "(^|[^<])<<-?[[:space:]]*[\"" q "]?[A-Za-z_][A-Za-z0-9_]*[\"" q "]?" }
       # unquoted(s): s (the text before a heredoc operator) ends outside any quotes, tracking
       # backslash escapes, so `echo "<<EOF"` and `echo \" "<<EOF"` are not heredocs. `<<<` (a
       # here-string) is excluded by the operator pattern.
       function unquoted(s,   i, c, sq, dq) {
         sq = 0; dq = 0
         for (i = 1; i <= length(s); i++) {
           c = substr(s, i, 1)
           if (sq) { if (c == q) sq = 0; continue }
           if (c == "\\") { i++; continue }
           if (c == "\"") dq = !dq
           else if (c == q && !dq) sq = 1
         }
         return !sq && !dq
       }
       FNR == 1 { buf = ""; hd = ""; hdp = "" }
       hd != "" { t = $0; sub(/^\t+/, "", t); if (t == hd) hd = ""; next }
       /^[[:space:]]*#/ { next }
       { if (buf == "") start = FNR
         line = $0
         if (match(line, hdre) && unquoted(substr(line, 1, RSTART))) {
           tag = substr(line, RSTART, RLENGTH); sub(/^[^<]?<<-?[[:space:]]*/, "", tag); gsub("[\"" q "]", "", tag); hdp = tag }
         if (sub(/\\$/, "", line)) { buf = buf line " "; next }
         if (line ~ /(\||&&)[[:space:]]*$/) { buf = buf line " "; next }
         print FILENAME ":" start ":" buf line; buf = ""
         if (hdp != "") { hd = hdp; hdp = "" } }' "$@" 2>/dev/null
}
_GIT_GREP="git( +-[Cc] +(\"[^\"]*\"|'[^']*'|[^ ]+))* +grep"
gitgrep_gnuisms() { logical_lines "$@" | grep -E "${_GIT_GREP}"'.*\\[b<>]' || true; }
gitgrep_into_quiet_grep() { logical_lines "$@" | grep -E "${_GIT_GREP}"'[^;]*\| *grep( +-[A-Za-z]+)* +-[A-Za-z]*q' || true; }
GG_FILES=("$ROOT_DIR"/scripts/tests/*.sh "$ROOT_DIR"/scripts/tests/lib/*.sh "$ROOT_DIR"/scripts/*.sh "$ROOT_DIR"/scripts/gen/*.sh "$ROOT_DIR"/.githooks/pre-push)
GG="$(gitgrep_gnuisms "${GG_FILES[@]}")"
if [ -z "$GG" ]; then ok "no git-grep pattern uses \\b, \\< or \\> (portable boundaries only)"
else no "git grep patterns depend on the regex engine: $(cut -c1-160 <<<"$GG" | tr '\n' ';')"; fi
GQ="$(gitgrep_into_quiet_grep "${GG_FILES[@]}")"
if [ -z "$GQ" ]; then ok "no git grep is piped into a short-circuiting grep (capture, then test)"
else no "git grep piped into 'grep -q' (its SIGPIPE can decide the leg): $(cut -c1-160 <<<"$GQ" | tr '\n' ';')"; fi
# Controls, built from pieces so this file's own text never matches.
_gg_dir="$(mktemp -d)"
_GG='git'" grep"; _PG='|'" grep"; _B='\'"b"
printf '%s\n' "if $_GG -nE 'synthetic|Foo${_B}' -- x; then :; fi" > "$_gg_dir/alt.sh"
printf '%s\n' "OUT=\"\$($_GG -nE \\" "  'synthetic|Foo${_B}' -- x)\"" > "$_gg_dir/cont.sh"
printf '%s\n' "if git -C . grep -nE 'Foo${_B}' -- x; then :; fi" > "$_gg_dir/dashc.sh"
printf '%s\n' "OUT=\"\$($_GG -nE '(^|[^A-Za-z0-9_])Foo([^A-Za-z0-9_]|\$)' -- x)\"" > "$_gg_dir/portable.sh"
printf '%s\n' "if $_GG -n Foo -- x $_PG -q .; then :; fi" > "$_gg_dir/quiet.sh"
printf '%s\n' "if $_GG -n Foo -- x \\" "   | sed 's/a/b/' $_PG -qE . ; then :; fi" > "$_gg_dir/quiet_cont.sh"
printf '%s\n' "if git -C \"a b\" grep -nE 'Foo${_B}' -- x; then :; fi" > "$_gg_dir/dashc_quoted.sh"
printf '%s\n' "# a comment ending in a backslash \\" "if $_GG -nE 'Foo${_B}' -- x; then :; fi" > "$_gg_dir/after_comment.sh"
printf '%s\n' "if $_GG -n Foo -- x $_PG -E -q .; then :; fi" > "$_gg_dir/quiet_split_opts.sh"
printf '%s\n' "if $_GG -n Foo -- x |" "   grep -q .; then :; fi" > "$_gg_dir/quiet_pipe_cont.sh"
printf '%s\n' "OUT=\"\$($_GG -n Foo -- x)\"; printf '%s' \"\$OUT\" $_PG -q ." > "$_gg_dir/capture_then_test.sh"
printf '%s\n' "cat > f <<'EOF'" "$_GG -nE 'Foo${_B}' -- x $_PG -q ." "EOF" > "$_gg_dir/heredoc.sh"
for c in alt cont dashc dashc_quoted after_comment; do
  if [ -n "$(gitgrep_gnuisms "$_gg_dir/$c.sh")" ]; then ok "control: a \\b git grep pattern ($c) is flagged"
  else no "control: a \\b git grep pattern ($c) was NOT flagged (the check above is inert)"; fi
done
if [ -z "$(gitgrep_gnuisms "$_gg_dir/portable.sh")$(gitgrep_into_quiet_grep "$_gg_dir/portable.sh")" ]; then
  ok "control: a captured search with portable bracket-expression boundaries is not flagged"
else no "control: a captured, portable search was flagged"; fi
printf '%s\n' "echo \"<<EOF\"" "if $_GG -n Foo -- x $_PG -q .; then :; fi" > "$_gg_dir/after_quoted_heredoc_text.sh"
printf '%s\n' "cat <<<EOF" "if $_GG -n Foo -- x $_PG -q .; then :; fi" > "$_gg_dir/after_here_string.sh"
printf '%s\n' "echo \\\" \"<<EOF\"" "if $_GG -n Foo -- x $_PG -q .; then :; fi" > "$_gg_dir/after_escaped_quote_heredoc_text.sh"
printf '%s\n' "if $_GG -n Foo -- x |" "  # why the next stage is here" "  grep -q .; then :; fi" > "$_gg_dir/comment_in_pipeline.sh"
for c in quiet quiet_cont quiet_split_opts quiet_pipe_cont after_quoted_heredoc_text after_here_string after_escaped_quote_heredoc_text comment_in_pipeline; do
  if [ -n "$(gitgrep_into_quiet_grep "$_gg_dir/$c.sh")" ]; then ok "control: git grep piped into grep -q ($c) is flagged"
  else no "control: git grep piped into grep -q ($c) was NOT flagged (the check above is inert)"; fi
done
for c in capture_then_test heredoc; do
  if [ -z "$(gitgrep_gnuisms "$_gg_dir/$c.sh")$(gitgrep_into_quiet_grep "$_gg_dir/$c.sh")" ]; then
    ok "control: $c is not flagged (a separate test after a capture; heredoc data)"
  else no "control: $c was flagged"; fi
done
rm -rf "$_gg_dir"

echo ""
echo "=== no pipe into a short-circuiting grep or head before a match (bug 283) ==="
# `producer | grep -q PAT` under `pipefail` lets the verdict depend on scheduling: grep exits at
# its first match, a producer still writing dies of SIGPIPE (141), and the pipeline reports that
# death. A positive leg then FAILs with the pattern present (CI run 40269354812); a negated leg
# (`... && no || ok`) PASSES with the forbidden text present, silently. The hazard is the SHAPE, a
# consumer that stops reading, not the producer: an elaborator, git grep and bash's own printf have
# each lost the race. So the rule covers every producer:
#   * no pipe into grep/egrep/fgrep with -q, -m, -l or -L (or --quiet, --silent, --max-count,
#     --files-with-matches, --files-without-match); match captured text with
#     `has_match PAT <<<"$OUT"` and a stream with `producer | has_match PAT`
#     (scripts/tests/lib/has_match.sh), which reads all of its input before it answers;
#   * no `| head` (or `| sed ...q`) feeding has_match: it closes the pipe early in the same way.
#     Cap with `awk 'NR<=N'`, which reads everything.
# `grep -c` reads all of its input, so a count is not refused here. Scanned as logical lines
# (logical_lines above): comments and heredoc bodies are skipped, continuations are joined.
# Each logical line is read three ways: as written; with quoted text blanked (blank_quotes), so a
# pattern or an option argument between grep and its -q (`grep -e PAT -q`, `grep 'a|b' -q`) cannot
# hide it; and each quoted string's own text, blanked the same way (quoted_texts), so a leg spelled
# inside an `eval` or `bash -c` string is read as the code it is. The blanked views over-approximate:
# `grep -e -q` (a pattern spelled like the option) is flagged too; spell such a pattern otherwise.
_QG='(^|[^|])\|&?[[:space:]]*[ef]?grep([[:space:]]+(-[A-Za-z0-9]+|--[a-z-]+(=[^[:space:]]*)?))*[[:space:]]+(-[A-Za-z0-9]*[qmlL][A-Za-z0-9]*([[:space:]]|$)|--(quiet|silent|max-count|files-with))'
_QG_ANY='(^|[^|])\|&?[[:space:]]*[ef]?grep([[:space:]][^|;&]*)?[[:space:]](-[A-Za-z0-9]*[qmlLV]|--(q|s|m|files-with|c|v|he))'
_HM='(^|[^|])\|&?[[:space:]]*(head([[:space:]][^;&]*)?|sed[[:space:]][^|;&]*q[^;&]*)\|&?[[:space:]]*has_match([[:space:]]|$)'
blank_quotes() { # each line with every quoted string replaced by Q, or by its text when that is one
                 # dash-led word (a quoted option, `grep "-q"`, is still an option)
  awk 'BEGIN { q = sprintf("%c", 39) }
       { s = $0; n = length(s); out = ""; i = 1
         while (i <= n) {
           c = substr(s, i, 1)
           if (c == "\\") { out = out substr(s, i, 2); i += 2; continue }
           if (c == q) { j = index(substr(s, i + 1), q); if (j == 0) { out = out "Q"; break }
             out = out word(substr(s, i + 1, j - 1)); i += j + 1; continue }
           if (c == "\"") { k = i + 1
             while (k <= n) { d = substr(s, k, 1); if (d == "\\") { k += 2; continue }; if (d == "\"") break; k++ }
             out = out word(substr(s, i + 1, k - i - 1)); i = k + 1; continue }
           out = out c; i++ }
         print out }
       function word(t) { return (t ~ /^-[^ \t]*$/) ? t : "Q" }'
}
quoted_texts() { # FILE:LINE:text -> FILE:LINE:<the text inside each quoted string>, one per line
  awk 'BEGIN { q = sprintf("%c", 39) }
       { if (!match($0, /^[^:]*:[0-9]+:/)) next
         pre = substr($0, 1, RLENGTH); s = substr($0, RLENGTH + 1); n = length(s); i = 1
         while (i <= n) {
           c = substr(s, i, 1)
           if (c == "\\") { i += 2; continue }
           if (c == q) { j = index(substr(s, i + 1), q); if (j == 0) break
             print pre substr(s, i + 1, j - 1); i += j + 1; continue }
           if (c == "\"") { k = i + 1
             while (k <= n) { d = substr(s, k, 1); if (d == "\\") { k += 2; continue }; if (d == "\"") break; k++ }
             print pre substr(s, i + 1, k - i - 1); i = k + 1; continue }
           i++ } }'
}
pipe_into_short_circuit() {
  local lines
  lines="$(logical_lines "$@")"
  [ -n "$lines" ] || return 0
  { grep -E "$_QG|$_HM" <<<"$lines"
    blank_quotes <<<"$lines" | grep -E "$_QG_ANY|$_HM"
    quoted_texts <<<"$lines" | blank_quotes | grep -E "$_QG_ANY|$_HM"; } || true
}
# A script that calls has_match must load it (directly, or through lib/gate_env.sh): an undefined
# has_match is "command not found", status 127, which a negated leg would read as "absent".
has_match_unloaded() {
  local f
  for f in "$@"; do
    [ "${f##*/}" = has_match.sh ] && continue
    has_match -E '(^|[^A-Za-z0-9_.-])has_match([[:space:]]|$)' <<<"$(logical_lines "$f")" || continue
    grep -qE '(lib/gate_env\.sh|/has_match\.sh)' "$f" || echo "$f"
  done
}
SC_FILES=("$ROOT_DIR"/scripts/*.sh "$ROOT_DIR"/scripts/*/*.sh "$ROOT_DIR"/scripts/tests/lib/*.sh "$ROOT_DIR"/.githooks/pre-push)
SC="$(pipe_into_short_circuit "${SC_FILES[@]}")"
if [ -z "$SC" ]; then ok "no script pipes into a short-circuiting grep or through head into a match"
else no "pipe into a short-circuiting consumer (SIGPIPE can decide the leg; use has_match): $(cut -c1-200 <<<"$SC" | tr '\n' ';')"; fi
UL="$(has_match_unloaded "${SC_FILES[@]}")"
if [ -z "$UL" ]; then ok "every script that calls has_match loads it"
else no "has_match called without loading lib/has_match.sh or lib/gate_env.sh: $(tr '\n' ' ' <<<"$UL")"; fi

# Controls for the scan, built from pieces so this file's own text never matches.
_sc_dir="$(mktemp -d)"
_P='|'; _G='grep'
printf '%s\n' "printf '%s' \"\$OUT\" $_P $_G -q PAT && ok x || no x" > "$_sc_dir/printf_q.sh"
printf '%s\n' "printf '%s' \"\$OUT\" $_P $_G -q FORBIDDEN && no x || ok x" > "$_sc_dir/negated.sh"
printf '%s\n' "make_report $_P $_G -qE 'a|b'" > "$_sc_dir/stream_qE.sh"
printf '%s\n' "make_report $_P $_G -E -q x" > "$_sc_dir/split_opts.sh"
printf '%s\n' "make_report $_P" "  # why" "  $_G -qF x" > "$_sc_dir/continued.sh"
printf '%s\n' "make_report \\" "  $_P $_G -q x" > "$_sc_dir/backslash.sh"
printf '%s\n' "v=\"\$(make_report $_P $_G -m1 x)\"" > "$_sc_dir/max_count.sh"
printf '%s\n' "make_report $_P $_G -l x" > "$_sc_dir/files_with.sh"
printf '%s\n' "make_report $_P $_G --quiet x" > "$_sc_dir/long_quiet.sh"
printf '%s\n' "make_report $_P e$_G -q x" > "$_sc_dir/egrep.sh"
printf '%s\n' "make_report $_P head -1 $_P has_match x" > "$_sc_dir/head_then_match.sh"
printf '%s\n' "make_report $_P sed 5q $_P has_match x" > "$_sc_dir/sedq_then_match.sh"
printf '%s\n' "make_report $_P $_G -e PAT -q" > "$_sc_dir/pattern_arg_then_q.sh"
printf '%s\n' "make_report $_P $_G -A 2 -q PAT" > "$_sc_dir/context_arg_then_q.sh"
printf '%s\n' "make_report $_P $_G PAT -q" > "$_sc_dir/pattern_then_q.sh"
printf '%s\n' "make_report $_P $_G 'a|b' -q" > "$_sc_dir/quoted_alt_then_q.sh"
printf '%s\n' "check x 'make_report $_P $_G -q PAT'" > "$_sc_dir/in_eval_string.sh"
printf '%s\n' "make_report $_P head -1 $_P sed s/a/b/ $_P has_match x" > "$_sc_dir/head_filter_match.sh"
printf '%s\n' "make_report $_P $_G -c -e ' -q' && echo \"a $_P b -l\"" > "$_sc_dir/quoted_option_text.sh"
printf '%s\n' "make_report $_P sed '5q' $_P has_match x" > "$_sc_dir/quoted_sedq_then_match.sh"
printf '%s\n' "check x 'make_report $_P $_G -e PAT -q'" > "$_sc_dir/in_eval_pattern_then_q.sh"
printf '%s\n' "check x \"make_report $_P $_G -q 'PAT'\"" > "$_sc_dir/in_dq_string.sh"
printf '%s\n' "make_report $_P $_G PAT --qui" > "$_sc_dir/abbreviated_long.sh"
printf '%s\n' "make_report $_P $_G --q PAT" > "$_sc_dir/shortest_quiet.sh"
printf '%s\n' "make_report $_P $_G --s PAT" > "$_sc_dir/shortest_silent.sh"
printf '%s\n' "make_report $_P $_G --m=1 PAT" > "$_sc_dir/shortest_max_count.sh"
printf '%s\n' "make_report $_P $_G -m+1 PAT" > "$_sc_dir/signed_max_count.sh"
printf '%s\n' "make_report $_P $_G -V PAT" > "$_sc_dir/version.sh"
printf '%s\n' "make_report $_P $_G --he" > "$_sc_dir/help_abbrev.sh"
printf '%s\n' "make_report $_P $_G \"-q\" PAT" > "$_sc_dir/dq_option.sh"
printf '%s\n' "make_report $_P $_G '--q' PAT" > "$_sc_dir/sq_long_option.sh"
printf '%s\n' "make_report $_P $_G -q;x" > "$_sc_dir/option_then_semicolon.sh"
printf '%s\n' "make_report $_P$_P $_G -q x f" > "$_sc_dir/or_list.sh"
printf '%s\n' "has_match -E 'a|b' <<<\"\$OUT\" && ok x || no x" > "$_sc_dir/herestring.sh"
printf '%s\n' "make_report $_P has_match -F x" > "$_sc_dir/drained.sh"
printf '%s\n' "$_G -q x \"\$f\" && $_G -c y f" > "$_sc_dir/file_operand.sh"
printf '%s\n' "make_report $_P $_G -c x" > "$_sc_dir/count.sh"
printf '%s\n' "make_report $_P awk 'NR<=1' $_P has_match x" > "$_sc_dir/awk_cap.sh"
printf '%s\n' "cat > f <<'EOF'" "make_report $_P $_G -q x" "EOF" > "$_sc_dir/heredoc.sh"
printf '%s\n' "# make_report $_P $_G -q x" > "$_sc_dir/comment.sh"
for c in printf_q negated stream_qE split_opts continued backslash max_count files_with long_quiet egrep head_then_match sedq_then_match \
         pattern_arg_then_q context_arg_then_q pattern_then_q quoted_alt_then_q in_eval_string head_filter_match \
         quoted_sedq_then_match in_eval_pattern_then_q in_dq_string abbreviated_long shortest_quiet shortest_silent \
         shortest_max_count signed_max_count version help_abbrev dq_option sq_long_option option_then_semicolon; do
  if [ -n "$(pipe_into_short_circuit "$_sc_dir/$c.sh")" ]; then ok "control: a pipe into a short-circuiting consumer ($c) is flagged"
  else no "control: a pipe into a short-circuiting consumer ($c) was NOT flagged (the scan is inert)"; fi
done
for c in or_list herestring drained file_operand count awk_cap heredoc comment quoted_option_text; do
  if [ -z "$(pipe_into_short_circuit "$_sc_dir/$c.sh")" ]; then ok "control: $c is not flagged"
  else no "control: $c was flagged"; fi
done
printf '%s\n' "has_match x <<<\"\$OUT\" && no x || ok x" > "$_sc_dir/unloaded.sh"
printf '%s\n' "source \"\$ROOT/scripts/tests/lib/has_match.sh\"" "has_match x <<<\"\$OUT\" && no x || ok x" > "$_sc_dir/loaded.sh"
[ -n "$(has_match_unloaded "$_sc_dir/unloaded.sh")" ] && ok "control: a script calling has_match without loading it is flagged" \
  || no "control: a script calling has_match without loading it was NOT flagged"
[ -z "$(has_match_unloaded "$_sc_dir/loaded.sh")" ] && ok "control: a script that sources has_match.sh is not flagged" \
  || no "control: a script that sources has_match.sh was flagged"

# The bug and the fix, executed. A producer far larger than a pipe buffer with the match on its
# first line: grep -q exits after its first read, and the producer's next write gets EPIPE.
_HMLIB="$ROOT_DIR/scripts/tests/lib/has_match.sh"
cat > "$_sc_dir/race.sh" <<'RACE'
set -uo pipefail
source "$1"
BIG="$(seq 1 500000)"
# The inversion counts only when grep really matched (status 0) and the producer died writing:
# status 141 when SIGPIPE kills it, or 1 when the runner starts jobs with SIGPIPE ignored and printf
# reports EPIPE as a write error (GitHub's Linux runners). Both statuses are read from PIPESTATUS at
# once. A grep that failed (status 2) gives a non-zero pipeline too, without proving anything.
OLD_PAT="${2:-^1\$}"
inverted() { # producer-status grep-status -> true when the pipeline lost a real match to the producer
  case "$1" in 141|1) [ "$2" = 0 ] ;; *) false ;; esac
}
printf '%s' "$BIG" 2>/dev/null | grep -q -- "$OLD_PAT"; ps=("${PIPESTATUS[@]}")
echo "old-positive-status=producer:${ps[0]} grep:${ps[1]}"
inverted "${ps[0]}" "${ps[1]}" && echo "old-positive=inverted" || echo "old-positive=not-inverted"
printf '%s' "$BIG" 2>/dev/null | grep -q -- "$OLD_PAT"; ps=("${PIPESTATUS[@]}")
echo "old-negated-status=producer:${ps[0]} grep:${ps[1]}"
# The negated leg `... && no || ok` passes on any non-zero pipeline; inverted when grep did match.
inverted "${ps[0]}" "${ps[1]}" && echo "old-negated=passed-on-forbidden" || echo "old-negated=not-inverted"
printf '%s' "$BIG" | has_match '^1$'; echo "pipe-positive=$?"
printf '%s' "$BIG" | has_match '^1$' && echo "pipe-negated=forbidden-seen" || echo "pipe-negated=passed"
has_match '^1$' <<<"$BIG"; echo "here-positive=$?"
has_match '^1$' <<<"$BIG" && echo "here-negated=forbidden-seen" || echo "here-negated=passed"
has_match '^0$' <<<"$BIG"; echo "absent=$?"
{ echo hit; exit 3; } | has_match hit; echo "producer-failure=$?"
RACE
RACE_OUT="$(bash "$_sc_dir/race.sh" "$_HMLIB" 2>&1)" || true
for want in old-positive=inverted old-negated=passed-on-forbidden; do
  if grep -qxF "$want" <<<"$RACE_OUT"; then ok "control: the old pipe reproduces bug 283 ($want; $(grep -E '^old-(positive|negated)-status=' <<<"$RACE_OUT" | tr '\n' ' '))"
  else no "control: the old pipe did not reproduce bug 283 ($want missing): $(tr '\n' ' ' <<<"$RACE_OUT")"; fi
done
# The reproduction control refuses a grep that failed: with an invalid pattern the old pipeline is
# non-zero too (grep status 2), and that must not read as the inversion.
BAD_OUT="$(bash "$_sc_dir/race.sh" "$_HMLIB" '\(' 2>&1)" || true
if grep -qE '^old-positive-status=producer:[0-9]+ grep:2$' <<<"$BAD_OUT" && grep -qxF 'old-positive=not-inverted' <<<"$BAD_OUT" \
   && grep -qxF 'old-negated=not-inverted' <<<"$BAD_OUT"; then
  ok "control: a failing grep (invalid pattern, status 2) is not read as the inversion"
else no "control: a failing grep was read as the inversion, or did not fail: $(grep '^old-' <<<"$BAD_OUT" | tr '\n' ' ')"; fi
for want in pipe-positive=0 pipe-negated=forbidden-seen here-positive=0 here-negated=forbidden-seen absent=1 producer-failure=3; do
  if grep -qxF "$want" <<<"$RACE_OUT"; then ok "control: has_match on the same producer: $want"
  else no "control: has_match on the same producer: $want missing: $(tr '\n' ' ' <<<"$RACE_OUT")"; fi
done
# A grep error, or an option that would stop grep early anywhere in the arguments, is no verdict
# in either polarity: from the script's own shell it exits 2, and from a pipeline stage it kills
# the script (137), even one that traps TERM and would carry on.
for form in "2|has_match -E '(' <<<x" "137|printf x | has_match -E '('" \
            "137|printf x | has_match -E '(' && echo verdict=forbidden || echo verdict=clean" \
            "137|trap ':' TERM; printf x | has_match -E '(' && echo verdict=forbidden || echo verdict=clean" \
            "2|has_match -q x <<<x" "137|seq 1 500000 | has_match -e '^1\$' -q" "137|seq 1 500000 | has_match '^1\$' --max-count=1" \
            "137|seq 1 500000 | has_match '^1\$' --qui && echo verdict=forbidden || echo verdict=clean" \
            "137|seq 1 500000 | has_match -V absent && echo verdict=forbidden || echo verdict=clean" \
            "2|has_match x /etc/hostname <<<x" "2|has_match -e x /etc/hostname <<<x"; do
  want="${form%%|*}"; form="${form#*|}"
  erc=0; eout="$(bash -c "source '$_HMLIB'; set -o pipefail; $form; echo reached" 2>&1)" || erc=$?
  if [ "$erc" = "$want" ] && ! grep -qE '^(reached|verdict=)' <<<"$eout" && grep -qF 'has_match:' <<<"$eout"; then
    ok "control: no verdict, status $want ($form)"
  else no "control: an error or a refused option was not stopped as status $want ($form; rc=$erc): $(tr '\n' ' ' <<<"$eout")"; fi
done
# Options that take an argument are read past, so a pattern spelled like a refused option is not one.
if bash -c "source '$_HMLIB'; set -o pipefail; seq 1 500000 | has_match -f /dev/null; [ \$? = 1 ] && has_match -equiet <<<'xquietx' && has_match -A 1 -e x <<<x && ! has_match -Fe m <<<x && has_match --regexp -q <<<'a-qb' && has_match --fixed-strings --regexp=. <<<'a.b'" 2>/dev/null; then
  ok "control: has_match reads option arguments as arguments (-equiet, -A 1, -Fe m, --regexp -q, --regexp=.)"
else no "control: has_match refused or misread an option argument"; fi
rm -rf "$_sc_dir"

echo ""
echo "=== a summary line and the exit status cannot disagree ==="
# A gate emitting `PASS=.. FAIL=..` must derive its status from that count rather than
# ending in a literal non-zero exit that ignores it.
DISAGREE=""
for g in "${HGATES[@]}"; do
  grep -qE 'echo "[A-Z0-9-]+: *PASS=' "$g" || continue
  t5="$(tail -5 "$g")"
  grep -qE '\[ "\$FAIL" -(eq|ne) 0 \]|exit \$\(\( *FAIL|\[ "\$\{FAIL' <<<"$t5" && continue
  grep -qE '^exit [1-9]' <<<"$t5" && DISAGREE="$DISAGREE $(basename "$g")"
done
if [ -z "$DISAGREE" ]; then
  ok "every gate with a PASS/FAIL summary derives its exit status from that count"
else
  no "gate(s) end in a literal non-zero exit despite printing a summary:$DISAGREE"
fi

echo ""
echo "=== those three controls are tested against broken fixtures ==="
# A clean corpus and an inert lint look identical without this.
HTMP="$(mktemp -d)"
printf '%s\n' 'if [ 1 = 1 ]; then echo hi' 'fi' 'fi' > "$HTMP/parse_bad.sh"
bash -n "$HTMP/parse_bad.sh" 2>/dev/null \
  && no "CONTROL: an invalid fixture parsed cleanly — the parse control is inert" \
  || ok "CONTROL: the parse control rejects an unbalanced fi"

# The token is assembled, not written literally: a bare `exit 1` at column zero in THIS file
# is indistinguishable to the stranded-assertion control from the defect it hunts, and this
# lint flagged itself on first run. Exempting the file would leave it outside its own rule.
ETOK="$(printf %s exit) 1"
{ echo 'PASS=0'; echo "$ETOK"; echo 'ok "never runs"'; echo 'ok "nor this"'; } > "$HTMP/early.sh"
el="$(grep -nE '^exit [0-9]+$' "$HTMP/early.sh" | awk 'NR<=1' | cut -d: -f1)"
ea="$(awk -v n="$el" 'NR>n' "$HTMP/early.sh" | grep -cE '^[[:space:]]*(ok|no) "')"
[ "${ea:-0}" -gt 0 ] \
  && ok "CONTROL: the stranded-assertion control sees $ea unreachable assertions" \
  || no "CONTROL: the stranded-assertion control is inert"

printf '%s\n' 'FAIL=0' 'echo "X: PASS=3 FAIL=$FAIL"' "$ETOK" > "$HTMP/disagree.sh"
dt="$(tail -5 "$HTMP/disagree.sh")"
if grep -qE '^exit [1-9]' <<<"$dt" && ! grep -qE '\[ "\$FAIL" -eq 0 \]' <<<"$dt"; then
  ok "CONTROL: the summary/exit control sees a contradicted summary"
else
  no "CONTROL: the summary/exit control is inert"
fi
printf '%s\n' 'FAIL=0' 'echo "X: PASS=3 FAIL=$FAIL"' '[ "$FAIL" -eq 0 ]' > "$HTMP/wellformed.sh"
wt="$(tail -5 "$HTMP/wellformed.sh")"
grep -qE '\[ "\$FAIL" -eq 0 \]' <<<"$wt" \
  && ok "CONTROL: a well-formed gate is NOT flagged" \
  || no "CONTROL: the summary/exit control flags a well-formed gate"
rm -rf "$HTMP"

# --- the campaign golden names the driver as it is in this tree ---
# check_campaign_summary_golden.sh pins the mutation driver's digest, but it runs a real campaign
# and only in a clone, so a push that edited the driver (cb8b9bac: one mutation anchor) reached CI
# with a stale golden. This compares just the recorded digest with the current file, every push,
# and names the regeneration command. The golden itself is still regenerated only by that command.
# shellcheck disable=SC1091
. "$ROOT_DIR/scripts/tests/lib/treestate.sh"
GOLDEN_DRV=$(sed -n 's/^repo_driver_sha=//p' "$ROOT_DIR/scripts/tests/fixtures/campaign-summary.golden")
TREE_DRV=$(ts_driver_digest "$ROOT_DIR")
if [ -n "$GOLDEN_DRV" ] && [ "$GOLDEN_DRV" = "$TREE_DRV" ]; then
  ok "the campaign golden's driver digest matches check_gate_mutation_coverage.sh"
else
  no "the campaign golden records driver ${GOLDEN_DRV:-<none>} but the tree's is $TREE_DRV — regenerate it with the command in check_campaign_summary_golden.sh's header (in a clone)"
fi
if [ -n "$TREE_DRV" ] && [ "$(printf 'x\n' | cat "$ROOT_DIR/scripts/tests/check_gate_mutation_coverage.sh" - | ts_digest)" != "$TREE_DRV" ]; then
  ok "CONTROL: a one-line driver change moves the digest"
else
  no "CONTROL: the driver digest does not see a driver change"
fi

echo ""
echo "=== no gate uses a fixed shared /tmp path (R-0546) ==="
# Gates run in parallel within one pass (run_ci_gates_local.sh JOBS>1) and several passes run at once
# on a shared host (concrete-ci-1 runs other agents' gate sets beside this one). A fixed name such as
# `/tmp/6c_sub.log` or `EXAMPLES_OUT_DIR=/tmp/concrete_examples_out` is shared by every concurrent
# writer, so one run's output can be read, truncated or deleted by another and the verdict becomes a
# function of the schedule. Scratch belongs in a per-run directory (`mktemp -d`, the suite's
# $TMPDIR) or at least a per-process name (`.$$`). A literal that is never written (a repro hint, a
# fixture string, recorded command text) is listed below with its reason.
declare -A TMP_OK=(
  ["run_tests.sh:/tmp/test_rerun"]="text of a '# Rerun:' hint printed for a human, never written by the suite"
  ["run_tests.sh:/tmp/test"]="text of a recorded repro command (save_failure), never written by the suite"
  ["check_native_libs.sh:/tmp/evil.so"]="fixture string inside a Concrete.toml under test; the compiler must refuse it"
  ["check_replay_producer.sh:/tmp/x"]="argument to a Lean #eval of a refusal message; nothing touches the path"
  ["capture_wrong_code_bundle.sh:/tmp/bug_bin"]="text written into a bundle's command.txt, not executed"
)
# Prints "file:line:/tmp/name" for each fixed /tmp path on a non-comment line. A token followed by
# `.$$`/`$$`, or on a line that calls mktemp, is per-run and not reported.
fixed_tmp_scan() {
  perl -ne '
    next if /^\s*#/ || /mktemp/;
    while (m{(?<![\w\$/{}-])(/tmp/[A-Za-z0-9_.-]*[A-Za-z0-9_])(\.?\$\$)?}g) {
      next if defined $2;
      my $t = $1; my $f = $ARGV; $f =~ s{.*/}{};
      print "$f:$.:$t\n";
    }
    close ARGV if eof;' "$@"
}
mapfile -t TMPSCAN < <(find "$ROOT_DIR/scripts/tests" "$ROOT_DIR/scripts/tests/lib" "$ROOT_DIR/scripts/ci" \
  -maxdepth 1 -type f -name '*.sh' ! -name 'check_gate_hygiene.sh' 2>/dev/null | LC_ALL=C sort -u)
# The examples' scripts (benchmarks, demos) run on the same shared hosts (bug 191: four bench.sh
# wrote fixed /tmp names).
mapfile -t -O "${#TMPSCAN[@]}" TMPSCAN < <(cd "$ROOT_DIR" && git ls-files 'examples/*.sh' 2>/dev/null \
  | sed "s|^|$ROOT_DIR/|" | LC_ALL=C sort -u)
TMPHITS=""
while IFS= read -r h; do
  [ -n "$h" ] || continue
  f="${h%%:*}"; t="${h##*:}"
  [ -n "${TMP_OK[$f:$t]:-}" ] && continue
  TMPHITS="$TMPHITS $h"
done < <(fixed_tmp_scan "${TMPSCAN[@]}")
# The workflow's gate commands are extracted and run by run_ci_gates_local.sh, so an env prefix that
# names a fixed /tmp path there (EXAMPLES_OUT_DIR=/tmp/... was one) is shared the same way.
while IFS= read -r h; do
  [ -n "$h" ] && TMPHITS="$TMPHITS lean_action_ci.yml:${h#*:}"
done < <(grep -nE '^[[:space:]]*(- )?run:.*scripts/.*\.(sh|py)' "$ROOT_DIR/.github/workflows/lean_action_ci.yml" \
           | perl -ne 'while (m{(?<![\w\$/{}-])(/tmp/[A-Za-z0-9_.-]*[A-Za-z0-9_])(\.?\$\$)?}g) { print "$1\n" unless defined $2 }')
if [ "${#TMPSCAN[@]}" -lt 100 ]; then
  no "only ${#TMPSCAN[@]} scripts enumerated for the fixed-/tmp scan — the check would be vacuous"
elif [ -z "$TMPHITS" ]; then
  ok "no fixed shared /tmp path in ${#TMPSCAN[@]} gate scripts or the workflow's gate commands"
else
  no "fixed shared /tmp path(s) — use mktemp -d or a .\$\$ suffix:$TMPHITS"
fi
TTMP="$(mktemp -d)"
printf '%s\n' 'echo x > /tmp/shared_name.log' > "$TTMP/fixed.sh"
printf '%s\n' 'D="$(mktemp -d /tmp/ok.XXXXXX)"' 'echo x > /tmp/ok_name.$$' '# echo > /tmp/comment_only' \
  'echo x > ${TMPDIR:-/tmp}/ok2.$$' > "$TTMP/perrun.sh"
[ -n "$(fixed_tmp_scan "$TTMP/fixed.sh")" ] \
  && ok "CONTROL: the fixed-/tmp scan flags a shared name" \
  || no "CONTROL: the fixed-/tmp scan is inert"
[ -z "$(fixed_tmp_scan "$TTMP/perrun.sh")" ] \
  && ok "CONTROL: the fixed-/tmp scan accepts mktemp, .\$\$ names and comments" \
  || no "CONTROL: the fixed-/tmp scan flags per-run paths: $(fixed_tmp_scan "$TTMP/perrun.sh" | tr '\n' ' ')"
rm -rf "$TTMP"

echo ""
echo "=== no test program or std test uses a fixed shared /tmp path (bug 191) ==="
# The same rule one level down: the PROGRAMS the suite runs. tests/programs/file_write_read.con
# wrote /tmp/concrete_test_io.txt and std's fs/io tests wrote /tmp/concrete_fs_test_*.txt. On a
# shared host the first user to run the suite owns those files; for every other user fopen returned
# NULL and the program segfaulted (test_ssa rc 139, `concrete test` rc 1). Test code builds its
# paths from CONCRETE_TEST_TMPDIR, which the runner sets to a directory private to the run
# (`concrete test` / `--test` in Main.lean runTestBinary, run_tests.sh, test_ssa.sh); std tests
# use std.test.scratch_path. A `/tmp/...` string literal in any .con source is refused unless it is
# listed here with the reason nothing touches the filesystem through it. Comments are not code: a
# path in a `//` or `///` line (a doc example) is not scanned.
declare -A CON_TMP_OK=(
  ["std/src/path.con:/tmp/x"]="PathBuf display test: the path is parsed and printed, never opened"
)
# Prints "path:line:<literal>" for each string literal of a .con source that names a /tmp path (paths
# relative to the repo root; the WHOLE literal, so an exemption covers exactly one literal). Each file is tokenized whole, as the lexer does, left to right: a char
# literal (one character or one escape, so a `'outer` loop label is a lone apostrophe and hides
# nothing), a string literal (which may span lines), or a `//` comment to the end of its line (the
# only comment form). A `//` or an apostrophe inside a string belongs to the string.
con_fixed_tmp_scan() {
  perl -0777 -ne '
    my $src = $_;
    while ($src =~ m{\x27(?:\\.|[^\x27\\\n])\x27|"((?:[^"\\]|\\.)*)"|//[^\n]*|\x27}gs) {
      next unless defined $1;
      my ($lit, $at) = ($1, $-[1]);
      # A /tmp path starts the literal or follows a separator (space, `=`, `:`, `,`, `;`, a quote
      # (an escaped `\"` too) or a bracket); after anything else (`build//tmp`, `x+/tmp`) it is part of a relative path.
      next unless $lit =~ m#(?:^|[\s=:,;"\x27(\[{])/tmp/#;
      my $line = 1 + (substr($src, 0, $at) =~ tr/\n//);
      (my $shown = $lit) =~ s/\n/\\n/g;
      print "$ARGV:$line:$shown\n";
    }' "$@"
}
mapfile -t CONSCAN < <(cd "$ROOT_DIR" && git ls-files '*.con' 2>/dev/null | LC_ALL=C sort)
CONHITS=""
if [ "${#CONSCAN[@]}" -gt 0 ]; then
  while IFS= read -r h; do
    [ -n "$h" ] || continue
    f="${h%%:*}"; rest="${h#*:}"; t="${rest#*:}"
    [ -n "${CON_TMP_OK[$f:$t]:-}" ] && continue
    CONHITS="$CONHITS $h"
  done < <(cd "$ROOT_DIR" && con_fixed_tmp_scan "${CONSCAN[@]}" || echo "SCANNER-FAILED:0:con_fixed_tmp_scan exited nonzero")
fi
# Every allowlist entry must still name a literal that exists, so a stale exemption cannot linger.
for k in "${!CON_TMP_OK[@]}"; do
  f="${k%%:*}"; t="${k#*:}"
  grep -qF "\"$t\"" "$ROOT_DIR/$f" 2>/dev/null || no "stale CON_TMP_OK entry $k: the literal is gone (remove the entry)"
done
if [ "${#CONSCAN[@]}" -lt 500 ]; then
  no "only ${#CONSCAN[@]} .con sources enumerated for the fixed-/tmp scan — the check would be vacuous"
elif [ -z "$CONHITS" ]; then
  ok "no fixed shared /tmp path in ${#CONSCAN[@]} .con sources (test programs, std tests, examples)"
else
  no "fixed shared /tmp path(s) in .con sources — build the path from CONCRETE_TEST_TMPDIR (std: std.test.scratch_path):$CONHITS"
fi
CTMP="$(mktemp -d)"
cat > "$CTMP/fixed.con" <<'CONEOF'
fn main() with(Std) -> Int {
    let path: String = "/tmp/shared_name.txt";
    drop_string(path);
    'outer: while true { let q: String = "/tmp/after_label.txt"; drop_string(q); break 'outer; }
    let c: char = '"';
    let r: String = "/tmp/after_quote_char.txt";
    let m: String = "/tmp/multi
line.txt";
    let x: String = "/tmp/x other.txt";
    let k: String = "key=/tmp/after_separator.txt";
    let q: String = "touch \"/tmp/quoted.txt\"";
    drop_string(r); drop_string(m); drop_string(x); drop_string(k); drop_string(q);
    return 0;
}
CONEOF
cat > "$CTMP/clean.con" <<'CONEOF'
// A doc example may name /tmp/example.txt in a comment.
/// let p: String = "/tmp/doc_example.txt";
fn main() with(Std) -> Int {
    let a: String = "data.txt"; // not "/tmp/trailing_comment.txt"
    let b: String = "/nonexistent/tmp/file.txt";
    let c: String = "x/tmp/y";
    let d: String = "build//tmp/file.txt";
    let e: String = "build+/tmp/file.txt";
    drop_string(a); drop_string(b); drop_string(c); drop_string(d); drop_string(e);
    return 0;
}
CONEOF
[ "$( (cd "$CTMP" && con_fixed_tmp_scan fixed.con) | tr '\n' ' ')" = "fixed.con:2:/tmp/shared_name.txt fixed.con:4:/tmp/after_label.txt fixed.con:6:/tmp/after_quote_char.txt fixed.con:7:/tmp/multi\\nline.txt fixed.con:9:/tmp/x other.txt fixed.con:10:key=/tmp/after_separator.txt fixed.con:11:touch \\\"/tmp/quoted.txt\\\" " ] \
  && ok "CONTROL: the .con fixed-/tmp scan flags /tmp literals (plain, after a loop label, after a '\"' char, spanning lines, an exempt prefix of a longer literal, after a separator or an escaped quote) whole, at their lines" \
  || no "CONTROL: the .con fixed-/tmp scan missed or misplaced a hit: $(con_fixed_tmp_scan "$CTMP/fixed.con" | tr '\n' ' ')"
[ -z "$(con_fixed_tmp_scan "$CTMP/clean.con")" ] \
  && ok "CONTROL: the .con fixed-/tmp scan ignores comments, doc examples and relative paths containing /tmp/" \
  || no "CONTROL: the .con fixed-/tmp scan flags non-shared text: $(con_fixed_tmp_scan "$CTMP/clean.con" | tr '\n' ' ')"
rm -rf "$CTMP"

echo ""
echo "=== every gate refuses to run outside the gate shell (lib/gate_env.sh) ==="
# Gates run in the dev shell, on Linux CI runners and on the macOS CI runner. lib/gate_env.sh refuses
# unless CONCRETE_GATES_SHELL=1, which only the flake's shells set (`nix develop .#gates`, the CI
# shell, and `nix develop`), so a gate never reports a verdict about some host's userland. That holds
# only if EVERY gate sources it, and sources it before doing anything else.
#
# A gate here is every shell script directly in scripts/tests, and every shell script a workflow names
# outside a comment, except the tools listed in GATE_TOOLS: scripts whose output is not a verdict.
# Naming a new script there is a decision with a reason; forgetting the guard is a failure here.
# GUARD_EXEMPT is the audited escape hatch for a script a workflow runs that must not need the shell
# (and is not a gate). Keep both short.
declare -A GUARD_EXEMPT=(
  ["scripts/dev/seed-lake.sh"]="build-cache seeding, not a gate: it checks its own toolchain/manifest key and only decides how long lake build takes, never a verdict"
  ["scripts/browser/build-site-compiler.sh"]="the site deployment's build of the browser compiler (book.yml, no gate shell there): a build step whose own parity tests fail it, not a gate verdict"
)
declare -A GATE_TOOLS=(
  ["scripts/tests/update_golden.sh"]="regenerates the golden baselines that test_golden.sh judges"
  ["scripts/tests/capture_release_bundle.sh"]="writes a release evidence bundle; test_release_bundle.sh judges it"
  ["scripts/tests/capture_wrong_code_bundle.sh"]="writes a wrong-code bundle; test_bundle_smoke.sh judges it"
  ["scripts/tests/minimize_wrong_code.sh"]="reduces a failing program; test_reducer_smoke.sh judges it"
  ["scripts/tests/triage_failure.sh"]="reduces and bundles one failing file for a person to read"
  ["scripts/tests/phase3_report_consistency_checks.sh"]="a snippet of check_report calls to paste into run_tests.sh, not runnable"
)
# _guard_first <file> <relative path of the library>: the file's first statement, or its second after
# an opening `set -...`, is the source line itself; anything earlier would run outside the guard.
# `|| exit 2` keeps a copy of the gate that cannot find the library (a snapshot, a sandbox) from
# running on unguarded. awk reads the file itself: a pipe into an early-exiting awk would SIGPIPE
# under pipefail.
_guard_first() {
  local first
  [ -n "$2" ] || return 1
  first="$(awk '/^[[:space:]]*(#|$)/ {next} !n++ && /^set -/ {next} {print; exit}' "$1")"
  [ "$first" = "source \"\$(dirname \"\${BASH_SOURCE[0]}\")/$2\" || exit 2" ]
}
GTMP="$(mktemp -d)"
GLINE='source "$(dirname "${BASH_SOURCE[0]}")/lib/gate_env.sh" || exit 2'
printf '#!/usr/bin/env bash\n# header\nset -euo pipefail\n%s\nok=1\n' "$GLINE" > "$GTMP/good.sh"
printf '#!/usr/bin/env bash\nROOT=x\n%s\n' "$GLINE" > "$GTMP/late.sh"
printf '#!/usr/bin/env bash\nsource "$(dirname "${BASH_SOURCE[0]}")/lib/gate_env.sh"\n' > "$GTMP/open.sh"
_guard_first "$GTMP/good.sh" lib/gate_env.sh \
  && ok "CONTROL: a gate whose first statement (after set -...) sources the guard is accepted" \
  || no "CONTROL: the guard check rejects a correctly guarded gate"
if _guard_first "$GTMP/late.sh" lib/gate_env.sh || _guard_first "$GTMP/open.sh" lib/gate_env.sh; then
  no "CONTROL: the guard check accepts a gate that runs code first, or sources the guard without '|| exit 2'"
else
  ok "CONTROL: a gate that runs code before the guard, or sources it without '|| exit 2', is flagged"
fi
rm -rf "$GTMP"
mapfile -t GUARDED < <( { cd "$ROOT_DIR" && for t in scripts/tests/*.sh; do [[ -n "${GATE_TOOLS[$t]:-}" ]] || echo "$t"; done
                         grep -hvE '^[[:space:]]*#' .github/workflows/*.yml | grep -oE 'scripts/[A-Za-z0-9_/.-]+\.sh'; } \
                       | LC_ALL=C sort -u)
if [ "${#GUARDED[@]}" -ge 250 ]; then
  ok "enumerated ${#GUARDED[@]} gates for the environment-guard check (floor 250)"
else
  no "only ${#GUARDED[@]} gates enumerated for the environment-guard check — it would be vacuous"
fi
UNGUARDED=""
for g in "${GUARDED[@]}"; do
  [ -f "$ROOT_DIR/$g" ] || { UNGUARDED="$UNGUARDED $g(missing)"; continue; }
  [[ -n "${GUARD_EXEMPT[$g]:-}" ]] && { echo "  skip $g (${GUARD_EXEMPT[$g]})"; continue; }
  case "$g" in
    scripts/tests/*/*) want="" ;;                      # no gate lives below scripts/tests
    scripts/tests/*) want="lib/gate_env.sh" ;;
    scripts/*/*/*) want="" ;;
    scripts/*/*) want="../tests/lib/gate_env.sh" ;;
    *) want="" ;;
  esac
  _guard_first "$ROOT_DIR/$g" "$want" || UNGUARDED="$UNGUARDED $g"
done
if [ -z "$UNGUARDED" ]; then
  ok "every gate sources lib/gate_env.sh as its first statement"
else
  no "gate(s) not guarded by lib/gate_env.sh as their first statement:$UNGUARDED"
fi
for t in "${!GATE_TOOLS[@]}"; do
  echo "  tool $t (${GATE_TOOLS[$t]})"
  [ -f "$ROOT_DIR/$t" ] || no "GATE_TOOLS names $t, which no longer exists — remove the entry"
done
for g in "${!GUARD_EXEMPT[@]}"; do
  [ -f "$ROOT_DIR/$g" ] && grep -qhF "$g" "$ROOT_DIR"/.github/workflows/*.yml \
    || no "GUARD_EXEMPT names $g, which no workflow runs any more — remove the entry"
done
# Behaviour, not just text: a real gate started without the marker stops with the message and exit 2,
# and the library lets a marked shell through.
gout="$(cd "$ROOT_DIR" && env -u CONCRETE_GATES_SHELL bash scripts/tests/check_build_cache.sh 2>&1)" && grc=0 || grc=$?
if [ "$grc" -eq 2 ] && grep -qF "run inside nix develop .#gates (CI) or nix develop" <<<"$gout"; then
  ok "a gate started outside the gate shell refuses (exit 2, names the shells)"
else
  no "a gate started outside the gate shell did not refuse (rc=$grc): $(head -1 <<<"$gout")"
fi
if [ "$(CONCRETE_GATES_SHELL=1 bash -c 'source "$1"; echo reached' _ "$ROOT_DIR/scripts/tests/lib/gate_env.sh" 2>&1)" = reached ]; then
  ok "CONTROL: inside the gate shell the guard lets the gate run"
else
  no "CONTROL: the guard refuses even with CONCRETE_GATES_SHELL=1"
fi

# The Makefile is the other way gates are started: each recipe that runs one enters the dev shell
# ($(NIX_DEVELOP) / $(NIX_DEVELOP_PROVERS)), or the recipe would now stop at the guard. `make pre-push`
# runs the hook, which re-enters the shell itself.
MKTMP="$(mktemp -d)"
# A LINT FOR USABILITY, not the enforcement: a recipe that slips past it still cannot report a
# verdict, because the gate it runs refuses outside the shell. It reads one COMMAND at a time, not one
# line: simple Makefile variables are expanded, continuations joined the way the shell joins them, a
# `target: ; recipe` read as a recipe, and each line split into the commands its &&, ||, ;, |
# separate outside quotes (so `$(NIX_DEVELOP) bash -c "a && b"` is one command). A command naming a
# script under scripts/{tests,ci,tools} must start with the shell, and may not reach the script
# through a command substitution, which the outer shell would run first.
cat > "$MKTMP/mk_check.py" <<'PYEOF'
import re, shlex, sys
GATE = re.compile(r"scripts/(tests|ci|tools)/")
SHELLS = ("NIX_DEVELOP", "NIX_DEVELOP_PROVERS")
src = open(sys.argv[1]).read()
var = {}
for m in re.finditer(r"^([A-Za-z_][A-Za-z0-9_]*)[ \t]*[:?+]?=[ \t]*(.*)$", src, re.M):
    if m.group(1) not in SHELLS:
        var[m.group(1)] = m.group(2)
def expand(t):
    for _ in range(4):
        t = re.sub(r"\$[({]([A-Za-z_][A-Za-z0-9_]*)[)}]",
                   lambda m: m.group(0) if m.group(1) in SHELLS else var.get(m.group(1), m.group(0)), t)
    return t
bad = []
for n, line in enumerate(re.sub(r"\\\n\t?", "", src).split("\n"), 1):
    m = re.match(r"^[^\t#][^:=]*:[^=;]*;(.*)$", line)
    recipe = m.group(1) if m else (line[1:] if line.startswith("\t") else None)
    if recipe is None:
        continue
    recipe = expand(recipe)
    if not GATE.search(recipe):
        continue
    body = re.sub(r"\$\((NIX_DEVELOP(?:_PROVERS)?)\)", r"\1", recipe.strip().lstrip("@-+").strip())
    try:
        lex = shlex.shlex(body, posix=False, punctuation_chars=";&|")
        lex.whitespace_split = True
        lex.commenters = ""
        toks = list(lex)
    except ValueError:
        bad.append(f"line {n}: cannot be parsed: {recipe.strip()}"); continue
    cmd = []
    for tok in toks + [";"]:
        if tok and set(tok) <= set(";&|"):
            text = " ".join(cmd)
            words = [w for w in cmd if not re.match(r"^[A-Za-z_][A-Za-z0-9_]*=", w)]
            if GATE.search(text) and (not (words and words[0] in SHELLS)
                                      or re.search(r"\$\$\(|`", text)):
                bad.append(f"line {n}: {text}")
            cmd = []
        else:
            cmd.append(tok)
print("\n".join(bad))
PYEOF
mk_bad="$(python3 -I "$MKTMP/mk_check.py" "$ROOT_DIR/Makefile")"
if [ -z "$mk_bad" ]; then
  ok "every Makefile recipe that runs a gate enters the dev shell"
else
  no "Makefile recipe(s) run a gate outside the dev shell: $(tr '\n' ' ' <<<"$mk_bad")"
fi
# CONTROLS: an unwrapped gate, and one chained after a wrapped gate, are flagged; a wrapped gate on a
# continuation line and a wrapped `bash -c` holding several gates are not.
cat > "$MKTMP/bad.mk" <<'MKEOF'
GATE = scripts/tests/check_foo.sh
a:
	bash ./scripts/tests/check_foo.sh
b:
	$(NIX_DEVELOP) bash scripts/tests/check_foo.sh && bash scripts/tests/check_bar.sh
c:
	python3 $(GATE)
d: ; python3 scripts/tests/x.py
e:
	echo foo#bar; python3 scripts/tests/x.py
f:
	$(NIX_DEVELOP) echo "$$(python3 scripts/tests/x.py)"
MKEOF
cat > "$MKTMP/good.mk" <<'MKEOF'
c:
	$(NIX_DEVELOP) bash \
	  scripts/tests/check_foo.sh
d:
	@$(NIX_DEVELOP) bash -c "bash scripts/tests/check_foo.sh && bash scripts/tests/check_bar.sh"
e:
	$(NIX_DEVELOP) bash -c "printf x" ";" scripts/tests/x.sh
f:
	cd x && $(NIX_DEVELOP) bash scripts/tests/x.sh
MKEOF
[ "$(python3 -I "$MKTMP/mk_check.py" "$MKTMP/bad.mk" | grep -c .)" = 6 ] \
  && ok "CONTROL: unwrapped, chained, variable-named, one-line-rule, after-#, and substituted gates are all flagged" \
  || no "CONTROL: the Makefile check missed an unwrapped or chained gate: $(python3 -I "$MKTMP/mk_check.py" "$MKTMP/bad.mk" | tr '\n' ' ')"
[ -z "$(python3 -I "$MKTMP/mk_check.py" "$MKTMP/good.mk")" ] \
  && ok "CONTROL: wrapped gates (continuation line, inside bash -c, after a quoted \";\", after cd) are accepted" \
  || no "CONTROL: the Makefile check flags a correctly wrapped recipe: $(python3 -I "$MKTMP/mk_check.py" "$MKTMP/good.mk" | tr '\n' ' ')"
rm -rf "$MKTMP"

echo ""
echo "=== every workflow run step executes in the gate shell ==="
# The guard above makes a gate refuse outside the shell; this keeps CI inside it. Each workflow that
# runs gates sets the gates shell as its default run shell, no job or step replaces it except the
# audited exceptions, and every job installs Nix on a GitHub-hosted runner before its first run step.
WTMP="$(mktemp -d)"
cat > "$WTMP/wf_check.py" <<'PYEOF'
import sys, yaml
root = sys.argv[1]
sys.path.insert(0, sys.argv[2])
from gates_shell import RUN_SHELL as SHELL
# job -> reason. Each step of these enters a superset shell itself.
SHELL_EXEMPT = {("lean_action_ci.yml", "multi-kernel"): "every step runs nix develop .#provers itself"}
bad = []
for wf in ("lean_action_ci.yml", "branch_health.yml"):
    d = yaml.safe_load(open(f"{root}/.github/workflows/{wf}"))
    if ((d.get("defaults") or {}).get("run") or {}).get("shell") != SHELL:
        bad.append(f"{wf}: workflow default run shell is not the gates shell")
    for name, job in d["jobs"].items():
        exempt = (wf, name) in SHELL_EXEMPT
        js = ((job.get("defaults") or {}).get("run") or {}).get("shell")
        if js is not None and not exempt:
            bad.append(f"{wf}:{name}: job overrides the run shell ({js})")
        installed = False
        for i, st in enumerate(job.get("steps", [])):
            if str(st.get("uses", "")).startswith("cachix/install-nix-action"):
                installed = True
            if "run" in st:
                if "shell" in st and not exempt:
                    bad.append(f"{wf}:{name}: step {st.get('name', i)!r} overrides the run shell")
                if not installed and not exempt:
                    bad.append(f"{wf}:{name}: step {st.get('name', i)!r} runs before Nix is installed")
for (wf, name), why in SHELL_EXEMPT.items():
    print(f"  skip {wf}:{name} ({why})")
for b in bad:
    print("  FAIL " + b)
sys.exit(1 if bad else 0)
PYEOF
python3 -I "$WTMP/wf_check.py" "$ROOT_DIR" "$ROOT_DIR/scripts/tests/lib" \
  && ok "CI and Branch health run every step in nix develop .#gates (Nix installed first on GitHub-hosted runners)" \
  || no "a workflow step runs outside the gates shell (above)"
# CONTROLS: the same check on a copy without the default shell, and on one whose first job lost its
# Nix install, must fail.
mkdir -p "$WTMP/nodef/.github/workflows" "$WTMP/noinst/.github/workflows"
cp "$ROOT_DIR/.github/workflows/branch_health.yml" "$WTMP/nodef/.github/workflows/"
cp "$ROOT_DIR/.github/workflows/branch_health.yml" "$WTMP/noinst/.github/workflows/"
sed '/^    shell: "nix develop .#gates/d' "$ROOT_DIR/.github/workflows/lean_action_ci.yml" > "$WTMP/nodef/.github/workflows/lean_action_ci.yml"
awk '!done && /uses: cachix\/install-nix-action/ {done=1; next} {print}' "$ROOT_DIR/.github/workflows/lean_action_ci.yml" > "$WTMP/noinst/.github/workflows/lean_action_ci.yml"
# Each must fail FOR ITS REASON: a crash (a traceback) would also exit nonzero.
wout="$(python3 -I "$WTMP/wf_check.py" "$WTMP/nodef" "$ROOT_DIR/scripts/tests/lib" 2>&1)" && wrc=0 || wrc=$?
if [ "$wrc" -eq 1 ] && grep -qF "lean_action_ci.yml: workflow default run shell is not the gates shell" <<<"$wout"; then
  ok "CONTROL: a workflow without the gates default shell is flagged"
else
  no "CONTROL: a workflow without the gates default shell was not flagged as such (rc=$wrc): $(tail -1 <<<"$wout")"
fi
wout="$(python3 -I "$WTMP/wf_check.py" "$WTMP/noinst" "$ROOT_DIR/scripts/tests/lib" 2>&1)" && wrc=0 || wrc=$?
if [ "$wrc" -eq 1 ] && grep -qF "runs before Nix is installed" <<<"$wout"; then
  ok "CONTROL: a job that runs a step before installing Nix is flagged"
else
  no "CONTROL: a job running a step before installing Nix was not flagged as such (rc=$wrc): $(tail -1 <<<"$wout")"
fi
rm -rf "$WTMP"

echo ""
echo "GATE-HYGIENE: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
