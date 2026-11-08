#!/usr/bin/env bash
# writer-round.sh MODEL CLONE BRANCH BRIEF OUT : one writer round, then commit any leftover tree and push to both remotes
model=$1 clone=$2 branch=$3 brief=$4 out=$5
cd "$clone" || exit 2
source scripts/checks/lib/bounded.sh
case $model in
  kimi)  bounded_run -r 60 14400 -- kimi -p "$(cat "$brief")" < /dev/null > "$out" 2>&1 ;;
  codex) bounded_run 14400 -- codex exec -s danger-full-access "$(cat "$brief")" < /dev/null > "$out" 2>&1 ;;
esac
echo "$model rc=$?" >> "$out"
[ "$(git branch --show-current)" = "$branch" ] || { echo "writer-round: not on $branch; nothing pushed" >> "$out"; exit 3; }
if [ -n "$(git status --porcelain)" ]; then
  git add -A && git commit -q -m "wip: end-of-round snapshot of the $model writer's uncommitted tree (not reviewed)" && echo "writer-round: committed leftover tree" >> "$out"
fi
for r in origin lambdaclass; do
  GIT_SSH_COMMAND="ssh -o ServerAliveInterval=20" git push "$r" "$branch" >> "$out.push" 2>&1 && echo "writer-round: pushed $branch to $r" >> "$out" || echo "writer-round: PUSH FAILED to $r (see $out.push)" >> "$out"
done
