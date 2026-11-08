ROOT_DIR="$1"; TMP="$(mktemp -d)"; trap 'rm -rf "$TMP"' EXIT
ok(){ echo "  ok   $1"; }; no(){ echo "  FAIL $1"; }
source "$ROOT_DIR/scripts/checks/lib/git_sandbox.sh"; git_sandbox
# acceptance's git shim (ISS-0017) refuses a push that could move main on a protected remote, however
# it is spelled, and lets a push to a throwaway local fixture through: the root checks the train's
# impact leg runs inside acceptance push main to such fixtures.
(
  source "$ROOT_DIR/scripts/land/acceptance_shim.sh"
  S="$TMP/shim"; mkdir -p "$S"; cd "$S"
  git init -q --bare prot.git; git init -q --bare fix.git
  make_git_shim "$S/bin" "$(command -v git)" "$S/prot.git" "https://example.invalid/mirror.git"
  G="$S/bin/git"
  git init -q -b main w; git -C w -c user.name=t -c user.email=t@t commit -q --allow-empty -m x
  git -C w branch wip/x
  git -C w remote add origin "$S/prot.git"; git -C w remote add fix "$S/fix.git"
  git -C w remote add mirror "https://example.invalid/mirror.git"
  git -C w remote add two "$S/fix.git"; git -C w remote set-url --add --push two "$S/fix.git"
  git -C w remote set-url --add --push two "$S/prot.git"
  bad=""
  for a in "fix main" "$S/fix.git main" "file://$S/fix.git main" "fix HEAD:refs/heads/main" "fix" \
           "origin wip/x" "-o main origin wip/x" "--repo=fix main"; do
    # shellcheck disable=SC2086
    "$G" -C w push -q $a 2>"$S/err" || bad="$bad [allowed refused: push $a: $(cat "$S/err")]"
  done
  for a in "origin main" "$S/prot.git main" "file://$S/prot.git main" "--repo=origin main" "origin HEAD" \
           "origin +wip/x:main" "origin wip/x:refs/heads/main" "origin refs/heads/main" "origin --all" "--mirror origin" \
           "-o ci.skip origin main" "origin" "two main" "mirror main" "../prot.git main" ""; do
    # shellcheck disable=SC2086
    if "$G" -C w push -q $a 2>"$S/err"; then bad="$bad [refused passed: push $a]"
    elif ! grep -q 'acceptance git shim: refusing' "$S/err"; then bad="$bad [push $a failed but not by the shim: $(cat "$S/err")]"; fi
  done
  "$G" -C w -c url."$S/prot.git".insteadOf=fixalias push -q fixalias main 2>"$S/err" && bad="$bad [insteadOf push passed]"
  [ -z "$(git -C prot.git for-each-ref)" ] || bad="$bad [the protected fixture gained refs: $(git -C prot.git for-each-ref --format='%(refname)' | tr '\n' ' ')]"
  [ -n "$(git -C fix.git rev-parse -q --verify refs/heads/main)" ] || bad="$bad [the local fixture has no main]"
  [ -z "$bad" ] && echo SHIM-OK || echo "SHIM-BAD:$bad"
) > "$TMP/shim.out" 2>&1
grep -qx SHIM-OK "$TMP/shim.out" \
  && ok "acceptance's git shim refuses every spelling of a push that could move main on a protected remote and lets local fixtures through" \
  || { no "acceptance git shim"; sed 's/^/      /' "$TMP/shim.out"; }
