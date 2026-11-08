#!/bin/bash
# Verbatim snapshot of gates-briefs (briefs, prompts, review outputs) into wip/gates-evidence-2026-11-08.
set -euo pipefail
B=~/projects/gates-briefs; E=~/projects/gates-evidence; D=$E/docs/proposals/gates-evidence
mkdir -p $D
find $B -maxdepth 1 -type f -exec cp -p {} $D/ \;
for x in prop n2 prop-agy prop-kimi prop-codex2 ro-A1 ro-B ro-B2 ro-C1 ro-D1; do
  [ -d $B/$x ] || continue; mkdir -p $D/$x
  for f in $B/$x/*.md; do b=$(basename $f)
    case $b in AGENTS.md|CLAUDE.md|README.md|CHANGELOG.md|ROADMAP.md) continue;; esac
    cp -p $f $D/$x/; done
done
cd $E; git add -A docs/proposals/gates-evidence
git diff --cached --quiet || git commit -qm "gates evidence: verbatim snapshot of briefs, prompts and review outputs ($(date -u +%H:%MZ))"
for r in origin lambdaclass; do timeout 900 env GIT_SSH_COMMAND="ssh -o ServerAliveInterval=20" git push -q $r HEAD:wip/gates-evidence-2026-11-08 2>&1 | grep -v "^pre-push" || true; done
git log -1 --oneline
