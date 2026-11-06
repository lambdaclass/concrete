#!/usr/bin/env bash
# SANDBOXES NEVER TOUCH THE CALLER'S REPOSITORY (ISS-0015).
#
# A pre-push hook of a linked worktree runs with GIT_DIR=<repo>/.git/worktrees/<name> exported. On
# 2026-11-06 review_tool.sh, run that way, did its sandbox work in the caller's clone and pushed 28 +
# 12 test notes to GitHub. The owner of the fix is the sandbox helper, scripts/checks/lib/
# git_sandbox.sh, which unsets every variable `git rev-parse --local-env-vars` names.
#
# Legs:
#   structural  every script under scripts/ that makes a repository
#               (git init, git clone, git worktree add) sources the helper and calls git_sandbox.
#   live        a throwaway CALLER (bare upstream, full clone, linked worktree; no hook installed
#               anywhere) runs review_tool.sh and legacy_frozen.sh, at this checkout's version, from
#               the linked worktree with GIT_DIR exported exactly as a hook sees it. The caller's
#               refs (refs/notes/* included), core.bare, worktree HEAD and its upstream's refs must be
#               byte-for-byte unchanged.
#   control     the same harness around a script that writes through an inherited GIT_DIR (as the
#               unfixed review_tool.sh did) must report the change, so the live leg cannot pass by
#               not looking.
#
# Every run is bounded: 120 s per attempt (bounded_run), and at most 512 tasks and 8G of memory (a
# systemd user scope, else both `ulimit -u` and `ulimit -v`; with neither the probe does not run and
# the check fails as incomplete). The
# 2026-11-06 reproducer had no such bound, recursed through a test hook and exhausted the machine.
set -uo pipefail
source "$(dirname "${BASH_SOURCE[0]}")/lib/gate_env.sh" || exit 2
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR" || exit 2
HELPER="scripts/checks/lib/git_sandbox.sh"
[ -f "$HELPER" ] || { echo "FATAL: $HELPER missing" >&2; exit 2; }
PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }

echo "=== structural: every repository-making script sources the sandbox helper ==="
# Two exceptions, neither a sandbox: install-bot.sh runs on the gate host as `bash -s <
# install-bot.sh` (no sibling files to source) and makes the bot's persistent clone, and
# worktree-new.sh makes a worktree OF the caller's repository by design.
EXEMPT=" scripts/land/install-bot.sh scripts/worktree-new.sh "
makers=0
for f in scripts/*.sh scripts/*/*.sh; do
  [ "$f" = scripts/checks/sandbox_isolation.sh ] && continue
  grep -qE 'git( -C [^ ]+)? (init|clone)\b|worktree add' "$f" || continue
  case "$EXEMPT" in *" $f "*) continue ;; esac
  makers=$((makers+1))
  # The helper must be sourced and called, as statements, before the first repository is made.
  first_make="$(grep -nE 'git( -C [^ ]+)? (init|clone)\b|worktree add' "$f" | grep -vE '^[0-9]+:[[:space:]]*#' | head -n 1 | cut -d: -f1)"
  src_line="$(grep -nE '^[[:space:]]*(source|\.) .*checks/lib/git_sandbox\.sh' "$f" | head -n 1 | cut -d: -f1)"
  call_line="$(grep -nE '^[[:space:]]*git_sandbox( |$)' "$f" | head -n 1 | cut -d: -f1)"
  if [ -n "$src_line" ] && [ -n "$call_line" ] && [ -n "$first_make" ] \
     && [ "$src_line" -lt "$call_line" ] && [ "$call_line" -lt "$first_make" ]; then
    ok "$f sources git_sandbox.sh (line $src_line) and calls it (line $call_line) before its first repository (line $first_make)"
  else
    no "$f makes a repository (line ${first_make:-?}) without sourcing (line ${src_line:-none}) and calling git_sandbox (line ${call_line:-none}) before it"
  fi
done
[ "$makers" -ge 9 ] && ok "found $makers repository-making scripts (at least the 9 known)" \
  || no "found only $makers repository-making scripts; the scan is not seeing them"

# The harness itself is a sandbox: it must not be steered by whatever environment started it.
source "$HELPER" || exit 2
TMP="$(mktemp -d)" || exit 2
trap 'rm -rf "$TMP"' EXIT
git_sandbox "$TMP/gitconfig" || exit 2
printf '[user]\n\tname = isolation gate\n\temail = isolation@example.invalid\n[init]\n\tdefaultBranch = main\n[advice]\n\tdetachedHead = false\n' > "$GIT_CONFIG_GLOBAL"

BOUNDED_PARENT="$TMP"
source "$ROOT_DIR/scripts/checks/lib/bounded.sh" || exit 2
# bounded <cmd...>: at most 120 s per attempt (the portable bounded_run: its own watchdog kills the
# whole process group; one re-run), at most 512 tasks and 8G of memory. The task and memory bound is
# a systemd user scope where one is available, else BOTH `ulimit -u` (this user's current process
# count + 512) and `ulimit -v` (8G); when neither can be set the command does not run at all and
# the result is 125, never an unbounded run.
SCOPE=0
if command -v systemd-run >/dev/null 2>&1 \
   && systemd-run --user --scope -q -p TasksMax=512 -p MemoryMax=8G true >/dev/null 2>&1; then SCOPE=1; fi
bounded() {
  if [ "$SCOPE" = 1 ]; then
    bounded_run -i /dev/null -r 120 120 -- systemd-run --user --scope -q -p TasksMax=512 -p MemoryMax=8G "$@"
  else
    local n; n="$(ps -o pid= -U "$(id -u)" 2>/dev/null | wc -l | tr -d ' ')"
    ( ulimit -u $(( ${n:-0} + 512 )) 2>/dev/null && ulimit -v $(( 8 * 1024 * 1024 )) 2>/dev/null || {
        echo "sandbox_isolation: cannot bound tasks and memory here (no systemd user scope; ulimit -u/-v refused); not running $*"; exit 125; }
      bounded_run -i /dev/null -r 120 120 -- "$@" )
  fi
}

# A caller: bare upstream, a full clone of this checkout's HEAD, a linked worktree of it, with this
# checkout's scripts/ (the version under test) copied over the worktree's. No hook is installed:
# hooksPath points at an empty directory.
mk_caller() { # dir
  local d="$1"
  mkdir -p "$d/nohooks"
  git clone -q --bare "$ROOT_DIR" "$d/up.git" || return 1
  git clone -q "$d/up.git" "$d/caller" || return 1
  git -C "$d/caller" config core.hooksPath "$d/nohooks"
  git -C "$d/caller" worktree add -q -b isolation-probe "$d/linked" || return 1
  cp -R "$ROOT_DIR/scripts/." "$d/linked/scripts/" || return 1
  git -C "$d/linked" add -A scripts && git -C "$d/linked" commit -q --allow-empty -m "scripts under test" || return 1
}
snap() { # dir
  local d="$1"
  git -C "$d/caller" for-each-ref --format='caller %(refname) %(objectname)'
  echo "caller core.bare=$(git -C "$d/caller" config core.bare)"
  echo "linked HEAD=$(git -C "$d/linked" rev-parse HEAD) $(git -C "$d/linked" symbolic-ref -q HEAD)"
  git -C "$d/caller" worktree list --porcelain | grep '^worktree '
  git -C "$d/up.git" for-each-ref --format='upstream %(refname) %(objectname)'
}
# run_as_hook <dir> <script relative to the linked worktree>: GIT_DIR exported as a linked
# worktree's hook sees it; prints the script's exit code.
run_as_hook() {
  local d="$1" s="$2" gd
  gd="$(git -C "$d/linked" rev-parse --absolute-git-dir)"
  ( cd "$d/linked" && GIT_DIR="$gd" && export GIT_DIR && bounded bash "$s" ) > "$d/run.log" 2>&1
  echo $?
}
probe() { # label script expect-change [completion-marker]
  local label="$1" s="$2" want="$3" marker="${4:-}" d rc
  d="$(mktemp -d "$TMP/caller.XXXXXX")"
  mk_caller "$d" || { no "$label: could not build the caller"; return; }
  [ -z "$(ls "$d/nohooks")" ] || { no "$label: a hook is installed"; return; }
  snap "$d" | LC_ALL=C sort > "$d/before"
  rc="$(run_as_hook "$d" "$s")"
  snap "$d" | LC_ALL=C sort > "$d/after"
  if [ "$want" = 0 ]; then
    [ "$rc" = 125 ] && { no "$label: INCOMPLETE: no task and memory bound available, so it was not run"; return; }
    bounded_is_incomplete "$rc" && { no "$label: INCOMPLETE: timed out twice (120 s each)"; tail -n 5 "$d/run.log" | sed 's/^/      /'; return; }
    [ "$rc" = 0 ] || { no "$label: exit $rc (an early failure would make 'unchanged' vacuous)"; tail -n 8 "$d/run.log" | sed 's/^/      /'; }
    grep -q "$marker" "$d/run.log" || { no "$label: its final tally ('$marker') is missing: it stopped early, so 'unchanged' proves nothing"; return; }
    if cmp -s "$d/before" "$d/after"; then ok "$label: the caller's refs, notes, config and upstream are unchanged"
    else no "$label: the caller changed"; diff "$d/before" "$d/after" | sed 's/^/      /' | head -20; fi
  else
    grep -q 'refs/notes/' "$d/after" && ! cmp -s "$d/before" "$d/after" \
      && ok "$label: the harness sees a write through the inherited GIT_DIR" \
      || no "$label: a leaking script left the caller unchanged; the harness is blind"
  fi
}

echo "=== control: the harness sees a leak ==="
mkdir -p "$TMP/ctl"
cat > "$TMP/ctl/leaky.sh" <<'EOF'
#!/usr/bin/env bash
# What the unfixed review_tool.sh did: build a sandbox without clearing GIT_DIR, then write to it.
T="$(mktemp -d)"; git init -q "$T/w"
git -C "$T/w" commit -q --allow-empty -m leaked
git -C "$T/w" notes --ref=refs/notes/concrete-review add -m "verdict: clean" HEAD
EOF
probe "control (leaky script)" "$TMP/ctl/leaky.sh" 1

echo "=== live: sandbox checks run from a linked worktree as its hook would run them ==="
probe "review_tool.sh" scripts/checks/review_tool.sh 0 "^check_review_tool: PASS=[0-9]* FAIL=0$"
probe "legacy_frozen.sh" scripts/checks/legacy_frozen.sh 0 "^legacy_frozen: PASS=[0-9]* FAIL=0$"

echo
bounded_summary
echo "check_sandbox_isolation: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
