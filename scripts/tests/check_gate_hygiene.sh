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
for f in check_*.sh test_*.sh; do
  [[ -n "${EXEMPT[$f]:-}" ]] && { echo "  skip $f (${EXEMPT[$f]})"; continue; }

  pipe_safe=false
  grep -q "lib/gate.sh" "$f" && pipe_safe=true
  grep -qE "set -[a-zA-Z]*o?[[:space:]]+pipefail|pipefail" "$f" && pipe_safe=true

  propagates=false
  # sourcing gate.sh brings gate_finish; otherwise require an explicit construct
  if grep -q "lib/gate.sh" "$f"; then
    propagates=true
  elif grep -qE "exit [1-9]|gate_finish|return 1|-eq 0[[:space:]]*\]|-gt 0|-ne 0|\|\|[[:space:]]*exit" "$f"; then
    propagates=true
  # A GATE THAT `exec`s ANOTHER GATE HAS THE STRONGEST PROPAGATION THERE IS. `exec` REPLACES this
  # process, so the delegate's exit status is this script's exit status with nothing in between to
  # lose it — there is no `exit 1` to find because there is no shell left to run one. Thin wrappers
  # that run one scoped case of an expensive gate, so a single refusal can be mutation-covered
  # without paying for the full suite, were being reported as unable to fail.
  elif grep -qE '^[[:space:]]*exec ([a-z]+ )*(env [A-Za-z_]+=[^ ]+ )*bash "?\$' "$f"; then
    propagates=true
  fi

  if $pipe_safe && $propagates; then
    ok "$f"
  elif ! $pipe_safe; then
    no "$f — not pipe-safe (add 'set -euo pipefail' or source lib/gate.sh)"
  else
    no "$f — no failure-exit construct (a detected failure won't set a nonzero exit code)"
  fi
done

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
  head -20 "$f" | grep -qE "set -[a-z]*e[a-z]* .*pipefail|set -o errexit" || continue
  # `[h]ead` so this scan does not match its own pattern literal.
  hits="$(grep -nE "\| *[h]ead -[0-9]" "$f" || true)"
  if [ -n "$hits" ]; then
    no "$(basename "$f") — '| head -N' under errexit+pipefail: $(head -1 <<<"$hits" | cut -d: -f1). Use '| awk \"NR<=N\"'"
    hazard=1
  fi
done
[ "$hazard" -eq 0 ] && ok "no errexit gate script caps output with '| head -N'"

echo ""
echo "=== no 'lake'/'lean' piped into a short-circuiting grep whose status is used ==="
# Same SIGPIPE family as the check above, but it INVERTS a gate leg instead of
# aborting the script. `lake env lean f.lean 2>&1 | grep -q PAT && ok || no`:
# grep exits at the first match, the elaborator is still writing, it dies of
# SIGPIPE, and under `pipefail` the pipeline's status is that death — so the leg
# reports FAIL against output that plainly contained PAT. Measured: a
# same-name-generator leg failed this way while the pattern was present.
#
# Scoped to lake/lean producers deliberately. `grep -q` after a fast, small
# producer finishes writing before grep exits and is fine; there are ~170 such
# uses in this directory and banning them all would be noise. An elaborator is
# slow and verbose, so it reliably loses the race.
#
# Fix: capture once into a variable, then grep the variable with a here-string.
# That is faster too — these gates elaborated the same file three times.
pipehazard=0
for f in "$ROOT_DIR"/scripts/tests/*.sh; do
  head -20 "$f" | grep -q "pipefail" || continue
  # Scan CODE only. Full-line comments are stripped first: prose describing this
  # very hazard matched it three times, and the same class (a docstring matching
  # a structural scan) has cost this tree several false failures already. Keeping
  # the line numbers means grep -n then filtering, not stripping in place.
  #
  # The producer must be an elaborator INVOCATION. A bare `lean ` alternative
  # also matched `Report.lean | grep -q…`, i.e. a filename followed by a pipe —
  # so the toolchain word must be anchored as a command.
  hits="$(grep -nE '^[0-9]+: *#' -v <<<"$(grep -n "" "$f")" \
    | grep -E "(lake +env +lean|lake +build|(^|[;(|&] *)lean +)[^|]*\| *grep -[a-zA-Z]*[q]" || true)"
  if [ -n "$hits" ]; then
    no "$(basename "$f") — elaborator piped into 'grep -q' under pipefail: line $(head -1 <<<"$hits" | cut -d: -f1). Capture to a variable first."
    pipehazard=1
  fi
done
[ "$pipehazard" -eq 0 ] && ok "no gate pipes an elaborator into a short-circuiting grep"

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
echo "=== concurrency guards are at the point of danger (docs/project/CONCURRENT_WORK.md) ==="
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
if grep -nE 'git push .*"(HEAD)?:?\$BRANCH"' "$ROOT_DIR/scripts/push-both.sh" | grep -q 'HEAD:'; then
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
echo "=== no git grep pattern depends on a regex-engine extension (\\b, \\<, \\>) ==="
# `git grep -E/-G` compiles its pattern with whatever regex library that git was built with, and
# the word-boundary escapes are extensions: BSD/macOS git treats `\b` as matching nothing, a GNU
# regex build treats it as a boundary. check_identity_separation.sh's C6 leg used
# `PackageIdentity\b`: inert on macOS git and in CI's regular jobs, red under Ubuntu 24.04's git
# 2.43 (GNU regex) on the same tree, and red in mutation shard 18 of run 39753399178, where the gate
# went RED ON CLEAN and two families scored INVALID; where `\b` is live it also matches the
# legitimate `noPackageIdentity` constructor and a history comment. A gate's verdict must not depend
# on which git it found. Plain grep(1) is not flagged: GNU and BSD grep agree on these. A line that
# runs git grep is flagged if \b, \< or \> appears anywhere on it (an alternation's `|` inside the
# quotes makes the pattern's end undecidable here), so keep a later plain grep on its own line.
gitgrep_gnuisms() { # files... -> offending lines (comments skipped)
  grep -HnE 'git( -C [^ ]+)? grep.*\\[b<>]' "$@" 2>/dev/null | grep -vE '^[^:]+:[0-9]+:[[:space:]]*#' || true
}
GG="$(gitgrep_gnuisms "$ROOT_DIR"/scripts/tests/*.sh "$ROOT_DIR"/scripts/tests/lib/*.sh "$ROOT_DIR"/scripts/*.sh "$ROOT_DIR"/scripts/gen/*.sh "$ROOT_DIR"/.githooks/pre-push \
  | grep -v '/check_gate_hygiene.sh:' || true)"
if [ -z "$GG" ]; then ok "no git grep pattern uses \\b, \\< or \\> (portable boundaries only)"
else no "git grep patterns depend on the regex engine: $(cut -c1-160 <<<"$GG" | tr '\n' ';')"; fi
_gg_fix="$(mktemp)"
# The shape that broke: an alternation (its `|` inside the quotes) ending in a \b.
printf '%s\n' "if git grep -nE 'synthetic|Foo\\b' -- x | grep -q .; then :; fi" > "$_gg_fix"
if [ -n "$(gitgrep_gnuisms "$_gg_fix")" ]; then ok "control: a git grep pattern with \\b is flagged"
else no "control: a git grep pattern with \\b was NOT flagged (the check above is inert)"; fi
printf '%s\n' "if git grep -nE '(^|[^A-Za-z0-9_])Foo([^A-Za-z0-9_]|\$)' -- x; then :; fi" > "$_gg_fix"
if [ -z "$(gitgrep_gnuisms "$_gg_fix")" ]; then ok "control: a portable bracket-expression boundary is not flagged"
else no "control: a portable boundary was flagged"; fi
rm -f "$_gg_fix"

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
echo "GATE-HYGIENE: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
