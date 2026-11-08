set -euo pipefail
REPO_URL=https://github.com/lambdaclass/concrete.git; H=$1; DRY_RUN=${DRY_RUN:-0}
die(){ echo "DIE: $*"; exit 1; }
run(){ if [ "$DRY_RUN" = 1 ]; then echo "would run: $*"; else "$@"; fi; }
as_dev(){ ( cd / && run sudo -u dev -H env "$@" ); }
dev_git() { ( cd / && sudo -u dev -H git -C "$H/concrete2" "$@" ); }
if [ -d "$H/concrete2/.git" ]; then
  # The clone's own settings (--local: what the writes below change); an inherited origin url, push
  # url or rewrite is not ours to edit and fails the effective check below.
  # A trailing sentinel keeps an empty entry (command substitution strips trailing newlines).
  urls="$(dev_git config --local --get-all remote.origin.url || true; echo .)"
  if ! dev_git remote | grep -qx origin; then as_dev git -C "$H/concrete2" remote add origin "$REPO_URL"
  elif [ "$urls" != "$REPO_URL"$'\n.' ]; then as_dev git -C "$H/concrete2" config --local --replace-all remote.origin.url "$REPO_URL"
  fi
  if dev_git config --local --get-all remote.origin.pushurl >/dev/null; then
    as_dev git -C "$H/concrete2" config --local --unset-all remote.origin.pushurl
  fi
  if [ "$DRY_RUN" != 1 ]; then
    eff="$(dev_git remote get-url --all origin; echo .; dev_git remote get-url --push --all origin; echo .)"
    [ "$eff" = "$REPO_URL"$'\n.\n'"$REPO_URL"$'\n.' ] \
      || die "~dev/concrete2 origin resolves to [$(echo $eff)], not $REPO_URL (an inherited origin url, push url or url.*.insteadOf in dev's git config?); fix it by hand"
  fi
fi
echo "urls=[$(git -C $H/concrete2 config --get-all remote.origin.url | tr "\n" ,)] push=[$(git -C $H/concrete2 config --get-all remote.origin.pushurl | tr "\n" , || true)]"
