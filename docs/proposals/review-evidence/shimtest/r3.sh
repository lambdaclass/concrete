ROOT_DIR="$1"; TMP="$(mktemp -d)"; trap 'rm -rf "$TMP"' EXIT
ok(){ echo "  ok   $1"; }; no(){ echo "  FAIL $1"; }
source "$ROOT_DIR/scripts/checks/lib/git_sandbox.sh"; git_sandbox
# acceptance's git shim (ISS-0017) refuses a push that could move main on a protected remote, however
# acceptance's git shim (ISS-0017) refuses a push that could move main on a protected repository,
# however it is spelled (remote name, URL, file://, a relative protected URL by its absolute path, a
# non-bare repository's .git, a symlink, a second push URL, an alias from config, -c or the
# environment, a shell alias), and lets a push to a throwaway local fixture through: the root checks
# the train's impact leg runs inside acceptance push main to such fixtures.
(
  source "/tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/shimtest/m.sh"
  S="$TMP/shim"; mkdir -p "$S"; cd "$S"
  git init -q --bare prot.git; git init -q --bare fix.git; git -C prot.git config receive.advertisePushOptions true
  git init -q -b other nb; ln -s prot.git link.git
  # The protected origin is given relative to the shim's directory, as a clone's relative URL would be.
  make_git_shim "$S/bin" "$(command -v git)" prot.git "$S/nb" "https://example.invalid/mirror.git"
  G="$S/bin/git"; export PATH="$S/bin:$PATH"
  git init -q -b main w; git -C w -c user.name=t -c user.email=t@t commit -q --allow-empty -m x
  git -C w branch wip/x
  git -C w remote add origin "$S/prot.git"; git -C w remote add fix "$S/fix.git"
  git -C w remote add mirror "https://example.invalid/mirror.git"
  git -C w remote add two "$S/fix.git"; git -C w remote set-url --add --push two "$S/fix.git"
  git -C w remote set-url --add --push two "$S/prot.git"
  git -C w config alias.p push; git -C w config alias.pp p; git -C w config alias.q "push -q"
  git -C w config alias.qq "push 'origin' main"; git -C w config alias.sh '!git push -q origin main'
  bad=""
  # shimref LABEL CMD...: CMD must fail, refused by the shim itself (not by git for another reason).
  shimref() { local l="$1"; shift; if "$@" 2>"$S/err"; then bad="$bad [$l passed]"
    elif ! grep -q 'acceptance git shim: refusing' "$S/err"; then bad="$bad [$l failed but not by the shim: $(cat "$S/err")]"; fi; }
  for a in "push -q fix main" "push -q $S/fix.git main" "push -q file://$S/fix.git main" "push -q fix HEAD:refs/heads/main" \
           "push -q fix :" "push -q origin wip/x" "push -q -o main origin wip/x" "push -q --repo=fix fix main" \
           "p -q fix main" "pp -q fix main" "q fix main" "-c alias.z=push z -q fix main" "status --short" \
           "-c alias.st=status st --short" "push -q file://localhost$S/fix.git main" \
           "push -q --force-with-lease fix main" "push -q --no-verify -- fix main"; do
    # shellcheck disable=SC2086
    "$G" -C w $a >/dev/null 2>"$S/err" || bad="$bad [allowed refused: git $a: $(cat "$S/err")]"
  done
  for a in "push origin main" "push $S/prot.git main" "push file://$S/prot.git main" "push --repo=origin main" \
           "push origin HEAD" "push origin +wip/x:main" "push origin wip/x:refs/heads/main" "push origin refs/heads/main" \
           "push origin --all" "push --mirror origin" "push -o ci.skip origin main" "push origin" "push two main" \
           "push mirror main" "push ../prot.git main" "push" "push origin @" "push origin :" "push origin +:" \
           "push origin refs/heads/*:refs/heads/*" "push origin wip/x:heads/main" "push origin :main" \
           "push --delete origin main" "push $S/nb/.git main" "push $S/nb main" "push file://$S/nb/.git main" \
           "push $S/link.git main" "p origin main" "pp origin main" "q origin main" "-c alias.z=push z origin main" \
           "qq" "sh" "-c url.$S/prot.git.insteadOf=fixalias push fixalias main" \
           "push --mir --receive-p git-receive-pack origin" "push -qf origin main" "push --branches origin wip/x" \
           "push --prune origin wip/x" "push --recurse-submodules=on-demand fix wip/x" \
           "-c push.recurseSubmodules=on-demand push fix wip/x" "push -- origin main"; do
    # shellcheck disable=SC2086
    if "$G" -C w $a >/dev/null 2>"$S/err"; then bad="$bad [refused passed: git $a]"
    elif ! grep -q 'acceptance git shim: refusing' "$S/err"; then bad="$bad [git $a failed but not by the shim: $(cat "$S/err")]"; fi
  done
  GIT_CONFIG_PARAMETERS="'alias.e=push'" shimref "an alias from the environment" "$G" -C w e origin main
  CFG=push shimref "an alias from --config-env" "$G" --config-env alias.e=CFG -C w e origin main
  CFG=push shimref "an alias from --config-env=" "$G" --config-env=alias.e=CFG -C w e origin main
  shimref "an unknown global option" "$G" --no-such-option -C w push origin main
  shimref "a push with submodule.recurse" "$G" -C w -c submodule.recurse=true push fix wip/x
  shimref "a bare --recurse-submodules before a remote named check" "$G" -C w -c remote.check.url="$S/prot.git" push --recurse-submodules check main wip/x
  shimref "a multi-line alias" "$G" -C w -c alias.ml="$(printf 'status\npush origin main')" ml
  shimref "a colon-less refspec mapped to main by remote.origin.push" "$G" -C w -c remote.origin.push=refs/heads/wip/x:refs/heads/main push origin wip/x
  mkdir -p w/target; ln -s "$S/prot.git" w/target.git
  shimref "a push to a path whose .git sibling is protected" "$G" -C w push "$S/w/target" main
  git -C prot.git symbolic-ref refs/heads/alias refs/heads/main
  shimref "a push to a symbolic ref for main" "$G" -C w push "$S/prot.git" wip/x:refs/heads/alias
  shimref "a push to a short symbolic ref for main" "$G" -C w push origin wip/x:alias
  git -C prot.git symbolic-ref -d refs/heads/alias; git -C prot.git symbolic-ref refs/tags/talias refs/heads/main
  shimref "a push to a short name that is a tag symbolic ref for main" "$G" -C w push --force origin wip/x:talias
  git -C prot.git symbolic-ref -d refs/tags/talias
  "$G" -C w push -q origin wip/x 2>"$S/err" || bad="$bad [a wip push to the protected origin with no symbolic refs refused: $(cat "$S/err")]"
  "$G" -C w push -q "$S/fix" main 2>"$S/err" || bad="$bad [a fixture push spelled without .git refused: $(cat "$S/err")]"
  # Protected local URLs in the other spellings git accepts: file://localhost/ and ~/.
  # And an absolute path holding "://", reached through a symlink.
  git init -q --bare p2.git; git init -q --bare p3.git; git init -q --bare "$S/store://p4.git"; ln -s "store:/p4.git" p4.git
  HOME="$S" make_git_shim "$S/bin2" "$(command -v git)" "file://localhost$S/p2.git" "~/p3.git" "$S/store://p4.git" \
    || bad="$bad [shim with file://localhost, ~ and :// path URLs not made]"
  for r in p2.git p3.git p4.git; do
    shimref "a push to the protected $r by its absolute path" "$S/bin2/git" -C w push -q "$S/$r" main
  done
  "$S/bin2/git" -C w push -q "$S/fix.git" main 2>"$S/err" || bad="$bad [a fixture push refused by the second shim: $(cat "$S/err")]"
  make_git_shim "$S/bin3" "$(command -v git)" "$S/no-such.git" 2>/dev/null && bad="$bad [a shim was made for a protected URL naming no repository]"
  make_git_shim "$S/bin3" "$(command -v git)" "file://otherhost/x.git" 2>/dev/null && bad="$bad [a shim was made for an unresolvable file:// URL]"
  for r in prot.git nb p2.git p3.git p4.git; do
    [ -z "$(git -C "$r" rev-parse -q --verify refs/heads/main)" ] || bad="$bad [the protected $r gained a main]"
  done
  [ -n "$(git -C fix.git rev-parse -q --verify refs/heads/main)" ] || bad="$bad [the local fixture has no main]"
  [ -z "$bad" ] && echo SHIM-OK || echo "SHIM-BAD:$bad"
) > "$TMP/shim.out" 2>&1
grep -qx SHIM-OK "$TMP/shim.out" \
  && ok "acceptance's git shim refuses every tested spelling of a push that could move main on a protected repository and lets local fixtures through" \
  || { no "acceptance git shim"; sed 's/^/      /' "$TMP/shim.out"; }
