#!/usr/bin/env bash
# mkreview.sh NAME CLONE SHA BASE "MODELS" DESCFILE : archive SHA, write REVIEW.diff + prompt, launch reviewers
set -eu
S=/tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad
name=$1 clone=$2 sha=$3 base=$4 models=$5 desc=$6
D=$S/rev-$name; mkdir -p "$D/src"
full=$(git -C "$clone" rev-parse "$sha")
git -C "$clone" archive "$full" | tar -x -C "$D/src"
git -C "$clone" diff "$base" "$full" > "$D/src/REVIEW.diff"
{ cat <<H
Read-only review. Do not modify, create or delete any file in this directory, do not run git, do not push or use the network. You may read files and run read-only or bounded local commands (temporary files only outside this directory).

The current directory is a plain copy (git archive) of the Concrete repository at commit $full. REVIEW.diff holds the change under review ($base..$full). Review it against the repository as a whole: read the files it touches and the code, docs, checks and decisions (docs/decisions.md, ROADMAP.md) it relies on.

H
cat "$S/longterm.txt"; echo
cat "$desc"; echo
cat <<'H'

Report findings with file:line, severity and the long-term fix. Start your answer by stating whether you could read the repository and list the files you read. End with exactly two lines:
REPOSITORY: readable   (or REPOSITORY: unreadable)
VERDICT: ACCEPT   (or VERDICT: REJECT)
H
} > "$D/prompt.txt"
for m in $models; do
  case $m in
    codex) systemd-run --user --unit=concrete-rev-$name-codex -p TasksMax=512 -p MemoryMax=8G --setenv=PATH="$PATH" --setenv=HOME="$HOME" -q bash $S/codex-pass.sh "$D/src" "$D/prompt.txt" "$D/codex.out" ;;
    agy) systemd-run --user --unit=concrete-rev-$name-agy -p TasksMax=512 -p MemoryMax=8G --setenv=PATH="$PATH" --setenv=HOME="$HOME" -q bash $S/agy-pass.sh "$D/src" "$D/prompt.txt" "$D/agy.out" ;;
    kimi) cat > "$D/kimi-run.sh" <<K
#!/usr/bin/env bash
source "\$HOME/projects/clones/signoff-record/scripts/checks/lib/bounded.sh"
cd "$D/src"; bounded_run 3600 -- kimi -p "\$(cat $D/prompt.txt)" < /dev/null > "$D/kimi.out" 2>&1; echo "rc=\$?" > "$D/kimi.out.rc"
K
      systemd-run --user --unit=concrete-rev-$name-kimi -p TasksMax=512 -p MemoryMax=8G --setenv=PATH="$PATH" --setenv=HOME="$HOME" -q bash "$D/kimi-run.sh" ;;
  esac
  echo "launched $m for $name at $full"
done
