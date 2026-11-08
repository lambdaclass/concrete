#!/usr/bin/env bash
# codex-pass.sh DIR PROMPTFILE OUT : full-access Codex in an archive copy; prompt saved verbatim beforehand.
source "$HOME/projects/clones/signoff-record/scripts/checks/lib/bounded.sh"
cd "$1" || exit 2
bounded_run 3600 -- codex exec --skip-git-repo-check -s danger-full-access "$(cat "$2")" < /dev/null > "$3" 2>&1
echo "rc=$?" > "$3.rc"
