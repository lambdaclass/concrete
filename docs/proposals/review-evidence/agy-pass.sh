#!/usr/bin/env bash
# agy-pass.sh SRC PROMPTFILE OUT : agy reviews inside the archive copy SRC with read access
source "$HOME/projects/clones/signoff-record/scripts/checks/lib/bounded.sh"
cd "$1" || exit 2
bounded_run 3600 -- agy --dangerously-skip-permissions -p "$(cat "$2")" < /dev/null > "$3" 2> "$3.err"
echo "rc=$?" > "$3.rc"
