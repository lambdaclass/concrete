#!/usr/bin/env bash
# The sandbox helper (ISS-0015). Every script that builds throwaway repositories sources this and
# calls git_sandbox before its first git command:
#
#   source "<path>/scripts/checks/lib/git_sandbox.sh" || exit 2
#   git_sandbox                      # repository isolation only (keeps the caller's git config)
#   git_sandbox "$TMP/gitconfig"     # also isolates config: GIT_CONFIG_GLOBAL=<file>, no system config
#
# Why: git exports the variables that locate a repository (GIT_DIR, GIT_WORK_TREE, GIT_INDEX_FILE,
# GIT_COMMON_DIR, GIT_OBJECT_DIRECTORY, GIT_CONFIG_PARAMETERS and the rest) to the hooks it runs, and
# a hook of a LINKED worktree sees GIT_DIR=<repo>/.git/worktrees/<name>. A script started under that
# environment that runs `git -C "$sandbox" ...` acts on the CALLER's repository, not the sandbox:
# on 2026-11-06 the review-tool check, run by a pre-push hook in a linked worktree, created its test
# branches in the caller's clone, set core.bare there, and pushed 28 + 12 test notes to GitHub's
# refs/notes/concrete-review and refs/notes/concrete-review-findings. The list comes from git itself
# (`git rev-parse --local-env-vars`), so it tracks whatever the installed git treats as local.

git_sandbox() { # [gitconfig-file]
  local v vars
  vars="$(env -u GIT_DIR -u GIT_WORK_TREE -u GIT_COMMON_DIR git rev-parse --local-env-vars 2>/dev/null)" \
    || { echo "git_sandbox: git rev-parse --local-env-vars failed" >&2; return 2; }
  [ -n "$vars" ] || { echo "git_sandbox: git listed no repository-local variables" >&2; return 2; }
  for v in $vars; do unset "$v"; done
  if [ $# -ge 1 ]; then
    if [ "${_CONCRETE_GIT_SANDBOX_CONFIG+x}" = x ] && [ "$_CONCRETE_GIT_SANDBOX_CONFIG" != "$1" ]; then
      echo "git_sandbox: conflicting isolated config: $_CONCRETE_GIT_SANDBOX_CONFIG (requested $1)" >&2
      return 2
    fi
    _CONCRETE_GIT_SANDBOX_CONFIG=$1
    export -n _CONCRETE_GIT_SANDBOX_CONFIG
    export GIT_CONFIG_GLOBAL="$1" GIT_CONFIG_NOSYSTEM=1
  fi
  return 0
}
