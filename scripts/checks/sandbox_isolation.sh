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
# Every run is bounded in time (120 s per attempt, bounded_run) and in aggregate: a systemd user
# scope with TasksMax=512 and MemoryMax=8G (fede-256gb, concrete-ci-2 logins), or, where there is
# none (a CI runner service), the cgroup the check already runs in when it has a finite memory.max
# (concrete-ci-2's runners: 28G per runner) plus `ulimit -u` of 512 more processes. With neither, the
# live probes do not run and the check fails as INCOMPLETE: never an unbounded run. macOS has
# neither (no unprivileged aggregate memory limit), so there the root checks are INCOMPLETE by design
# and a gated push from a macOS clone is refused: gated pushes (trains) are made from a Linux clone
# with a bound (fede-256gb or concrete-ci-2; no laptop builds), and CI runs on bounded runners. The 2026-11-06
# reproducer had no such bound, recursed through a test hook and exhausted the machine.
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
# One exception, not a sandbox: worktree-new.sh makes a worktree OF the caller's repository by design.
EXEMPT=" scripts/worktree-new.sh "
MAKES='git( -C [^ ]+)? (init|clone)\b|worktree add'
# shell_scripts DIR: every shell script under DIR/scripts at ANY depth, as paths relative to DIR,
# sorted: a `*.sh` file, or any other file whose first line is a sh/bash shebang. (Until 2026-11-08
# the scan was `scripts/*.sh scripts/*/*.sh`, so a script two directories down was never looked at.)
shell_scripts() {
  (cd "$1" && find scripts -type f -print | LC_ALL=C sort | while IFS= read -r f; do
    case "$f" in
      *.sh) echo "$f" ;;
      *) head -n 1 "$f" 2>/dev/null | grep -qE '^#!.*[/ ](ba)?sh([[:space:]]|$)' && echo "$f" ;;
    esac
  done)
}
# scan_makers DIR: one line per repository-making script under DIR (exemptions and this check
# skipped): `ok<TAB>file<TAB>why` or `FAIL<TAB>file<TAB>why`. The helper must be sourced and
# called, as statements, before the first repository is made.
scan_makers() {
  local dir="$1" f first_make src_line call_line
  python3 -I "$ROOT_DIR/scripts/checks/lib/python_git_sandbox.py" "$dir"
  shell_scripts "$dir" | while IFS= read -r f; do
    [ "$f" = scripts/checks/sandbox_isolation.sh ] && continue
    case "$EXEMPT" in *" $f "*) continue ;; esac
    grep -qE "$MAKES" "$dir/$f" || continue
    first_make="$(grep -nE "$MAKES" "$dir/$f" | grep -vE '^[0-9]+:[[:space:]]*#' | head -n 1 | cut -d: -f1)"
    src_line="$(grep -nE '^[[:space:]]*(source|\.) .*checks/lib/git_sandbox\.sh' "$dir/$f" | head -n 1 | cut -d: -f1)"
    call_line="$(grep -nE '^[[:space:]]*git_sandbox( |$)' "$dir/$f" | head -n 1 | cut -d: -f1)"
    if [ -n "$src_line" ] && [ -n "$call_line" ] && [ -n "$first_make" ] \
       && [ "$src_line" -lt "$call_line" ] && [ "$call_line" -lt "$first_make" ]; then
      printf 'ok\t%s\tsources git_sandbox.sh (line %s) and calls it (line %s) before its first repository (line %s)\n' "$f" "$src_line" "$call_line" "$first_make"
    else
      printf 'FAIL\t%s\tmakes a repository (line %s) without sourcing (line %s) and calling git_sandbox (line %s) before it\n' "$f" "${first_make:-?}" "${src_line:-none}" "${call_line:-none}"
    fi
  done
}
makers=0
while IFS=$'\t' read -r verdict f why; do
  makers=$((makers+1))
  if [ "$verdict" = ok ]; then ok "$f $why"; else no "$f $why"; fi
done < <(scan_makers "$ROOT_DIR")
[ "$makers" -ge 8 ] && ok "found $makers repository-making scripts (at least the 8 known)" \
  || no "found only $makers repository-making scripts; the scan is not seeing them"

echo "=== control: the structural scan sees scripts at any depth and without a .sh suffix ==="
# A planted tree: an unsandboxed maker three directories down and one with a bash shebang and no
# suffix must both FAIL; a sandboxed one at depth must pass. The scan cannot pass by not looking.
CTL_TREE="$(mktemp -d)" || exit 2
mkdir -p "$CTL_TREE/scripts/a/b/c"
printf '#!/usr/bin/env bash\ngit init -q "$1"\n' > "$CTL_TREE/scripts/a/b/c/deep.sh"
printf '#!/bin/bash\ngit clone -q "$1" "$2"\n' > "$CTL_TREE/scripts/a/b/nosuffix"
printf '#!/usr/bin/env bash\nsource "$(dirname "$0")/../../checks/lib/git_sandbox.sh"\ngit_sandbox "$1"\ngit init -q "$2"\n' > "$CTL_TREE/scripts/a/b/good.sh"
printf 'import subprocess\nsubprocess.run(["git", "init", "scratch"])\n' > "$CTL_TREE/scripts/a/b/c/leaky.py"
printf 'from protocol import clean_env\nimport os, subprocess\nsubprocess.run(["git", "init", "scratch"], env=clean_env(os.environ))\n' > "$CTL_TREE/scripts/a/b/c/safe.py"
ctl="$(scan_makers "$CTL_TREE")"; rm -rf "${CTL_TREE:?}"
printf '%s\n' "$ctl" | grep -q $'^FAIL\tscripts/a/b/c/deep.sh\t' \
  && ok "an unsandboxed maker three directories down is refused" || no "the scan missed scripts/a/b/c/deep.sh: $ctl"
printf '%s\n' "$ctl" | grep -q $'^FAIL\tscripts/a/b/nosuffix\t' \
  && ok "an unsandboxed maker with a bash shebang and no .sh suffix is refused" || no "the scan missed scripts/a/b/nosuffix: $ctl"
printf '%s\n' "$ctl" | grep -q $'^ok\tscripts/a/b/good.sh\t' \
  && ok "a sandboxed maker at depth passes" || no "the scan did not accept scripts/a/b/good.sh: $ctl"

printf '%s\n' "$ctl" | grep -q $'^FAIL\tscripts/a/b/c/leaky.py\tPython git subprocess requires protocol.clean_env environment (line 2)$' \
  && ok "Python inherited git environment is refused for its exact reason" || no "Python unsafe control missed: $ctl"
printf '%s\n' "$ctl" | grep -q $'^ok\tscripts/a/b/c/safe.py\t' \
  && ok "Python shared sanitizer is accepted" || no "Python safe control refused: $ctl"

# The harness itself is a sandbox: it must not be steered by whatever environment started it.
source "$HELPER" || exit 2
TMP="$(mktemp -d)" || exit 2
trap 'rm -rf "$TMP"' EXIT
git_sandbox "$TMP/gitconfig" || exit 2
printf '[user]\n\tname = isolation gate\n\temail = isolation@example.invalid\n[init]\n\tdefaultBranch = main\n[advice]\n\tdetachedHead = false\n' > "$GIT_CONFIG_GLOBAL"

BOUNDED_PARENT="$TMP"
source "$ROOT_DIR/scripts/checks/lib/bounded.sh" || exit 2
# bounded <cmd...>: at most 120 s per attempt (the portable bounded_run: its own watchdog kills the
# whole process group; one re-run), inside an aggregate task and memory bound (above). Without one
# the command does not run and the result is 125.
cgroup_memory_max() { # the finite memory.max of this process's cgroup v2 or an ancestor, else fails
  local cg f
  cg="$(sed -n 's/^0:://p' /proc/self/cgroup 2>/dev/null)"; [ -n "$cg" ] || return 1
  while :; do
    f="/sys/fs/cgroup$cg/memory.max"
    if [ -r "$f" ] && [ "$(cat "$f")" != max ]; then cat "$f"; return 0; fi
    [ "$cg" = / ] && return 1
    cg="$(dirname "$cg")"
  done
}
BOUND=none
if command -v systemd-run >/dev/null 2>&1 \
   && systemd-run --user --scope -q -p TasksMax=512 -p MemoryMax=8G true >/dev/null 2>&1; then BOUND=scope
elif CGMEM="$(cgroup_memory_max)"; then BOUND=cgroup; fi
case "$BOUND" in
  scope)  echo "bound: a systemd user scope, TasksMax=512, MemoryMax=8G" ;;
  cgroup) echo "bound: this cgroup's memory.max ($CGMEM bytes, aggregate) and ulimit -u (512 more processes)" ;;
  none)   echo "bound: none available (no systemd user scope, no finite cgroup memory.max; macOS has neither): the live probes will not run; run the root checks and gated pushes from a bounded Linux host (fede-256gb, concrete-ci-2) or in CI" ;;
esac
bounded() {
  case "$BOUND" in
    scope) bounded_run -i /dev/null -r 120 120 -- systemd-run --user --scope -q -p TasksMax=512 -p MemoryMax=8G "$@" ;;
    cgroup)
      local n; n="$(ps -o pid= -U "$(id -u)" 2>/dev/null | wc -l | tr -d ' ')"
      ( ulimit -u $(( ${n:-0} + 512 )) 2>/dev/null || { echo "sandbox_isolation: ulimit -u refused; not running $*"; exit 125; }
        bounded_run -i /dev/null -r 120 120 -- "$@" ) ;;
    *) echo "sandbox_isolation: no aggregate task and memory bound here; not running $*"; return 125 ;;
  esac
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
    [ "$rc" = 125 ] && { no "$label: INCOMPLETE: no task and memory bound available, so it was not run"; return; }
    bounded_is_incomplete "$rc" && { no "$label: INCOMPLETE: timed out twice"; return; }
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
