#!/bin/bash
# Every 10 min: push moved writer-branch tips to both remotes (never force). Pushes writer commits only.
L=~/projects/gates-briefs/backup-loop.log
for i in $(seq 60); do
  for p in gates-spec-diff:gate-spec-diff gates-scaling:gate-scaling-determinism gates-trusted-base:trusted-base gates-lcb-numbering:lcb-numbering gates-test-manifest:gates-manifest; do
    c=~/projects/${p%%:*}; b=wip/${p##*:}; h=$(git -C $c rev-parse HEAD)
    for r in origin lambdaclass; do
      t=$(git -C $c ls-remote $r refs/heads/$b 2>/dev/null | cut -f1)
      [ "$t" = "$h" ] && continue
      if [ -n "$t" ] && ! git -C $c merge-base --is-ancestor $t $h 2>/dev/null; then echo "$(date -u +%T) skip $b@$r (not fast-forward)" >>$L; continue; fi
      timeout 900 env GIT_SSH_COMMAND="ssh -o ServerAliveInterval=20" git -C $c push -q $r HEAD:$b >>$L 2>&1 && echo "$(date -u +%T) pushed $b@$r $h" >>$L
    done
  done
  sleep 600
done
