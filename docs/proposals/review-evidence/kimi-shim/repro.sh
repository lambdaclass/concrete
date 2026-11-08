#!/usr/bin/env bash
# Reproducer: acceptance.sh's git shim refuses a push of main to a throwaway local fixture remote,
# which the root checks (landing_bot.sh, landing_script.sh, review_tool.sh) do; so the impact leg
# fails inside every acceptance run. Usage: repro.sh CLONE (a checkout of origin/main).
set -u
C="$1"; W="$(mktemp -d)"; trap 'rm -rf "$W"' EXIT
# Extract make_git_shim from acceptance.sh exactly as it is and build the shim.
WORK="$W"; eval "$(sed -n '/^make_git_shim() {/,/^}/p' "$C/scripts/land/acceptance.sh")"; make_git_shim
export PATH="$W/bin:$PATH"
cd "$W"; git init -q --bare fixture.git; git init -q -b main r; cd r
git -c user.name=t -c user.email=t@t commit -q --allow-empty -m x
git remote add origin "$W/fixture.git"
if git push -q origin main 2>"$W/err"; then echo "NOT REPRODUCED: push of main to a local fixture passed"; exit 1; fi
echo "REPRODUCED: $(cat "$W/err")"
