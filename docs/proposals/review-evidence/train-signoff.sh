set -e
cd ~/land-rb
export GIT_SSH_COMMAND="ssh -o ServerAliveInterval=20"
T=scripts/land/train.sh
bash $T build signoff --force origin/wip/owner-signoff origin/wip/owner-signoff-split 2>&1 | tail -6
bash $T check signoff 2>&1 | tail -4
bash $T ci signoff 2>&1 | tail -3
bash $T watch signoff --timeout 5400 2>&1 | tail -2
bash $T --dry-run land signoff 2>&1 | tail -4
bash $T land signoff 2>&1 | tail -8
git log -1 --format='LANDED %H' origin/main
