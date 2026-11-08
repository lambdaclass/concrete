#!/bin/bash
# Runs the N2 round: Codex after Kimi supp ends, Kimi after agy supp ends, agy after Codex N2 ends. Max 2 of mine at once.
B=~/projects/gates-briefs; D=$B/n2
P=$(cat $D/N2ROUND.md)
waitpid_pat() { for i in $(seq 720); do pgrep -f "$1" >/dev/null || return 0; sleep 10; done; }
( waitpid_pat "kimi -S session_86329a83"
  cd $D && timeout 3600 systemd-run --user --scope -q -p TasksMax=256 -p MemoryMax=8G -- timeout 3500 codex exec -s danger-full-access --skip-git-repo-check -C $D -o $B/n2-codex.md "$P" > $B/n2-codex.log 2>&1; echo "codex rc=$?" >> $B/n2-codex.log ) &
( sleep 30; waitpid_pat "agy --dangerously-skip-permissions -p OWNER"
  cd $D && timeout 3600 systemd-run --user --scope -q -p TasksMax=256 -p MemoryMax=8G -- timeout 3500 kimi -p "$P" > $B/n2-kimi.log 2>&1; echo "kimi rc=$?" >> $B/n2-kimi.log ) &
wait %1
cd $D && timeout 3600 systemd-run --user --scope -q -p TasksMax=256 -p MemoryMax=8G -- timeout 3500 agy --dangerously-skip-permissions -p "$P" > $B/n2-agy.log 2>&1; echo "agy rc=$?" >> $B/n2-agy.log
wait
echo N2-QUEUE-DONE
