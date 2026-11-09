#!/usr/bin/env bash
# READINESS AND THE TRAIN'S REMOTE LEG, AGAINST A THROWAWAY REPOSITORY.
#
# Each leg drives the real scripts/land/ready.sh and train.sh against bare-repo remotes, a fake `gh`
# whose CI is red exactly when the candidate contains a file named CIRED, a fake remote runner, and a
# fake pre-push hook that fails exactly when the checkout contains a file named BAD. Nothing here
# touches this repository.
#
#   remote leg  train.sh check never adopts an existing ref at the candidate's SHA for its remote run
#   bisect --ci a CI-red member is named, each probe dispatching its own run, and nothing lands
#   ready.sh    its legs run; any failed leg, the review included, publishes `gates: fail` and revokes
#               readiness over an older clean verdict; passing legs publish `gates: pass`
#   main        no script under scripts/land force-pushes main
set -uo pipefail
source "$(dirname "${BASH_SOURCE[0]}")/lib/gate_env.sh" || exit 2
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
NOTES="$ROOT_DIR/scripts/land/notes.sh"
[ -f "$NOTES" ] || { echo "FATAL: scripts/land/notes.sh missing" >&2; exit 2; }
TMP="$(mktemp -d)" || { echo "FATAL: could not create a sandbox directory" >&2; exit 2; }
[ -n "$TMP" ] && [ -d "$TMP" ] || { echo "FATAL: sandbox directory is not usable" >&2; exit 2; }
trap 'rm -rf "$TMP"' EXIT
PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }
has(){ case "$1" in *"$2"*) return 0 ;; esac; return 1; }

source "$ROOT_DIR/scripts/checks/lib/git_sandbox.sh" || exit 2
git_sandbox "$TMP/gitconfig" || exit 2
cat > "$GIT_CONFIG_GLOBAL" <<EOF
[user]
	name = landing gate
	email = gate@example.invalid
[init]
	defaultBranch = main
[core]
	hooksPath = $TMP/nohooks
EOF
mkdir -p "$TMP/nohooks" "$TMP/gh" "$TMP/keys"
ssh-keygen -q -t ed25519 -N "" -C "orchestrator@concrete" -f "$TMP/keys/id_ed25519"
printf 'orchestrator@concrete namespaces="git" %s\n' "$(cat "$TMP/keys/id_ed25519.pub")" > "$TMP/keys/allowed_signers"
export REVIEW_WRITER_SIGNING_KEY="$TMP/keys/id_ed25519"
export REVIEW_WRITER_SIGNER="orchestrator@concrete"
export REVIEW_WRITER_ALLOWED_SIGNERS="$TMP/keys/allowed_signers"
# train.sh runs only with the repository's pre-push hook enabled (ISS-0015): it asks the hook git
# would run who it is (its single argument --concrete-identify). The sandbox's global hooksPath holds this
# stand-in, which runs no gate and answers as the repository's .githooks/pre-push would: a test
# fixture impersonating the hook on purpose.
IDENTIFY='[ "$#" = 1 ] && [ "$1" = --concrete-identify ] && { echo "concrete-pre-push $(cd "$(git rev-parse --show-toplevel)" && pwd -P)/.githooks/pre-push"; exit 0; }'
standin_hook() {
  printf '#!/usr/bin/env bash\n%s\nexit 0\n' "$IDENTIFY" > "$TMP/nohooks/pre-push"
  chmod +x "$TMP/nohooks/pre-push"
}
standin_hook
export LAND_PUSH_RETRY_SLEEP=0
export LAND_NO_NIX=1 LAND_STATE_DIR="$TMP/landstate" LAND_GH="$TMP/fake-gh" LAND_REPO=test/repo
export LAND_HOOK="$TMP/hook" LAND_REMOTE_RUNNER="$TMP/fake-runner" FAKE_GH_DIR="$TMP/gh" ORIGIN_GIT="$TMP/origin.git"
# train.sh check never skips the review leg silently. Every member here is marked reviewed clean
# (mark), so the leg passes on the members' provenance without calling the reviewer; a reviewer this
# fails stands for one that is never needed.
export LAND_REVIEW_CMD=false

cat > "$TMP/fake-gh" <<'GH'
#!/usr/bin/env bash
# CI is red exactly when the dispatched commit contains a file named CIRED.
echo "$*" >> "$FAKE_GH_DIR/calls.log"
case "$1 $2" in
  "workflow run")
    ref=""; while [ $# -gt 0 ]; do [ "$1" = --ref ] && ref="$2"; shift; done
    n=$(( $(cat "$FAKE_GH_DIR/counter" 2>/dev/null || echo 0) + 1 )); echo "$n" > "$FAKE_GH_DIR/counter"
    git --git-dir="$ORIGIN_GIT" rev-parse "refs/heads/$ref" > "$FAKE_GH_DIR/run.$n"
    exit 0 ;;
  "run list")
    [ -f "$FAKE_GH_DIR/list-fails" ] && { echo "fake gh: network down" >&2; exit 1; }
    [ -f "$FAKE_GH_DIR/list-empty" ] && { echo '[]'; exit 0; }
    n="$(cat "$FAKE_GH_DIR/counter")"
    printf '[{"databaseId": %s, "headSha": "%s", "createdAt": "9999-01-01T00:00:00Z"}]' "$n" "$(cat "$FAKE_GH_DIR/run.$n")" ;;
  "run view")
    sha="$(cat "$FAKE_GH_DIR/run.$3")"; c=success
    git --git-dir="$ORIGIN_GIT" cat-file -e "$sha:CIRED" 2>/dev/null && c=failure
    printf '{"status":"completed","conclusion":"%s","headSha":"%s","jobs":[{"name":"Build","conclusion":"%s","databaseId":%s}]}' "$c" "$sha" "$c" "$3" ;;
  "api "*) echo "fake log tail" ;;
  *) echo "fake gh: unexpected $*" >&2; exit 1 ;;
esac
GH
cat > "$TMP/hook" <<'EOF'
#!/usr/bin/env bash
cat > /dev/null
[ ! -e BAD ]
EOF
cat > "$TMP/fake-runner" <<'EOF'
#!/usr/bin/env bash
# A remote runner: fetches like concrete-remote (a ref on origin, or a SHA) and reports the SHA.
echo "$1" >> "$FAKE_GH_DIR/runner.log"
sha="$(git --git-dir="$ORIGIN_GIT" rev-parse --verify -q "refs/heads/$1" || git --git-dir="$ORIGIN_GIT" rev-parse --verify -q "$1^{commit}")" || { echo "no such ref $1"; exit 1; }
echo "[concrete-remote] ref=$1 sha=$sha"
EOF
chmod +x "$TMP/fake-gh" "$TMP/hook" "$TMP/fake-runner"

for r in origin lambdaclass fork; do git init -q --bare "$TMP/$r.git"; done
A="$TMP/agent"; W="$TMP/lander"
git init -q "$A"; cd "$A" || exit 2
for r in origin lambdaclass fork; do git remote add "$r" "$TMP/$r.git"; done
mkdir -p src
printf 'Build\n' > required-checks.txt  # train.sh refuses a landing with no required-checks.txt
git add -A && git commit -qm base
for r in origin lambdaclass fork; do git push -q "$r" main; done
BASE="$(git rev-parse HEAD)"
git clone -q "$TMP/origin.git" "$W"
( cd "$W" && git remote add lambdaclass "$TMP/lambdaclass.git" && git remote add fork "$TMP/fork.git" )

# mk <branch> <commit-message> <file> <content> [from]: one commit on <from> (default main), stamped as claude and pushed.
mk(){ ( cd "$A" && git fetch -q origin && git checkout -q -B "$1" "${5:-origin/main}" && mkdir -p "$(dirname "$3")" \
        && printf '%s\n' "$4" > "$3" && git add -A && git commit -qm "$2" \
        && python3 -I "$ROOT_DIR/scripts/review/writer_provenance.py" stamp-commit --repo "$A" --model claude --round 1 --branch "$1" --brief-sha256 2222222222222222222222222222222222222222222222222222222222222222 >/dev/null \
        && git push -q -f origin "HEAD:refs/heads/$1" refs/notes/concrete-writer ); }
sha_of(){ git --git-dir="$TMP/origin.git" rev-parse "refs/heads/$1"; }
mark(){ # branch [review-verdict] [gates] [extra review key=value]
  local s base repo=(); s="$(sha_of "$1")"; base="$(git --git-dir="$TMP/origin.git" rev-parse refs/heads/main)"
  [ "${2:-clean}" != clean ] || repo=(repository=readable)
  ( cd "$A" && git fetch -q origin \
    && bash "$NOTES" put review "$s" verdict="${2:-clean}" rounds=1 reviewer=codex writer=claude base="$base" pieces=1 "${repo[@]}" ${4:+"$4"} >/dev/null \
    && bash "$NOTES" put gates "$s" gates="${3:-pass}" selected=3 >/dev/null ) || no "mark $1"
}
land_refs(){ git --git-dir="$TMP/origin.git" for-each-ref --format='%(refname)' 'refs/heads/wip/land-*'; }

echo "=== remote leg: a same-SHA ref of another run is never adopted ==="
mk wip/cx cx src/cx.txt cx; mark wip/cx
( cd "$W" && git fetch -q origin && bash "$ROOT_DIR/scripts/land/train.sh" build cx-t --no-fetch origin/wip/cx >/dev/null 2>&1 \
  && git push -q origin "HEAD:refs/heads/wip/land-collide" )
CXH="$(cd "$W" && git rev-parse HEAD)"
: > "$TMP/gh/runner.log"
out="$(cd "$W" && LAND_REMOTE_REF_FIRST=wip/land-collide bash "$ROOT_DIR/scripts/land/train.sh" check cx-t --no-review 2>&1)"; rc=$?
rlog="$(cat "$LAND_STATE_DIR"/cx-t/check-*/remote.log 2>/dev/null)"
[ "$rc" = 0 ] && has "$rlog" "wip/land-collide was not created by this run" && ! has "$(cat "$TMP/gh/runner.log")" "wip/land-collide" \
  && [ "$(git --git-dir="$TMP/origin.git" rev-parse refs/heads/wip/land-collide)" = "$CXH" ] && [ "$(land_refs)" = "refs/heads/wip/land-collide" ] \
  && ok "an existing ref at the same SHA is refused, a fresh name is used, and only the run's own ref is deleted" || { no "collision: exit $rc, refs '$(land_refs)'"; echo "$rlog" | sed 's/^/      /'; }
# The run's own ref, positively: the runner ran exactly once, on a fresh wip/land-* ref (not the
# planted one), and that ref is gone.
rl="$(cat "$TMP/gh/runner.log")"
case "$rl" in wip/land-collide) fresh="" ;; wip/land-*) fresh="$rl" ;; *) fresh="" ;; esac
[ "$(printf '%s\n' "$rl" | grep -c .)" = 1 ] && [ -n "$fresh" ] \
  && ! git --git-dir="$TMP/origin.git" rev-parse -q --verify "refs/heads/$fresh" >/dev/null \
  && ok "the remote runner ran once, on the run's own fresh ref ($fresh), which is deleted afterwards" \
  || no "remote runner invocations: '$rl'"
git --git-dir="$TMP/origin.git" update-ref -d refs/heads/wip/land-collide
( cd "$W" && git checkout -q --detach origin/main )

echo "=== ready.sh: legs in parallel; any failed leg (the review included) publishes gates: fail ==="
mk wip/rv rv src/rv.txt rv; mark wip/rv
RV="$(sha_of wip/rv)"
out="$(cd "$A" && LAND_REVIEW_CMD=false bash "$ROOT_DIR/scripts/land/ready.sh" wip/rv 2>&1)"; rc=$?
g="$(cd "$A" && bash "$NOTES" get gates "$RV")"
[ "$rc" != 0 ] && has "$g" "gates: fail" && has "$g" "reason: failed legs: review" && has "$out" "run one after another" \
  && ok "a failed review leg revokes readiness even over an older clean verdict" || { no "ready.sh review failure: exit $rc, note '$g'"; echo "$out" | sed 's/^/      /'; }
mk wip/rv2 rv2 src/rv2.txt rv2; mark wip/rv2 clean fail
out="$(cd "$A" && LAND_REVIEW_CMD=true bash "$ROOT_DIR/scripts/land/ready.sh" wip/rv2 2>&1)"; rc=$?
[ "$rc" = 0 ] && [ "$(cd "$A" && bash "$NOTES" get gates "$(sha_of wip/rv2)" gates)" = pass ] \
  && ok "passing legs publish gates: pass and the head becomes ready" || { no "ready.sh pass: exit $rc"; echo "$out" | sed 's/^/      /'; }
# Readiness is judged through scripts/review/verify.sh (including --required-base): a legacy clean
# note, a self-reviewed note, a mismatched writer note, or a note based at an unreviewed intermediate
# commit B is refused by both notes.sh ready and ready.sh.
RV2_SHA="$(sha_of wip/rv2)"
( cd "$A" && bash "$NOTES" put review "$RV2_SHA" verdict=clean rounds=1 reviewer=codex writer=claude base="$BASE" pieces=1 >/dev/null && bash "$NOTES" fetch )
( cd "$A" && bash "$NOTES" ready "$RV2_SHA" ) && no "notes.sh ready accepted legacy clean note without repository: readable" || ok "notes.sh ready refuses a clean note without repository: readable"
out="$(cd "$A" && LAND_REVIEW_CMD=true bash "$ROOT_DIR/scripts/land/ready.sh" wip/rv2 2>&1)"; rc=$?
[ "$rc" != 0 ] && has "$out" "rejected by verify.sh" && ok "ready.sh refuses a clean note without repository: readable" || { no "ready.sh legacy clean note: exit $rc"; echo "$out" | sed 's/^/      /'; }
( cd "$A" && bash "$NOTES" put review "$RV2_SHA" verdict=clean rounds=1 reviewer=codex writer=codex base="$BASE" pieces=1 repository=readable >/dev/null && bash "$NOTES" fetch )
( cd "$A" && bash "$NOTES" ready "$RV2_SHA" ) && no "notes.sh ready accepted self-reviewed clean note" || ok "notes.sh ready refuses a self-reviewed clean note"
out="$(cd "$A" && LAND_REVIEW_CMD=true bash "$ROOT_DIR/scripts/land/ready.sh" wip/rv2 2>&1)"; rc=$?
[ "$rc" != 0 ] && has "$out" "rejected by verify.sh" && ok "ready.sh refuses a self-reviewed clean note" || { no "ready.sh self-reviewed clean note: exit $rc"; echo "$out" | sed 's/^/      /'; }
( cd "$A" && bash "$NOTES" put review "$RV2_SHA" verdict=clean rounds=1 reviewer=codex writer=agy base="$BASE" pieces=1 repository=readable >/dev/null && bash "$NOTES" fetch )
( cd "$A" && bash "$NOTES" ready "$RV2_SHA" ) && no "notes.sh ready accepted mismatched writer provenance" || ok "notes.sh ready refuses a note whose writer mismatches commit provenance"
mk wip/rvcov-b rvcov-b src/b.txt b
COV_B_SHA="$(sha_of wip/rvcov-b)"
mk wip/rvcov-c rvcov-c src/c.txt c wip/rvcov-b
COV_C_SHA="$(sha_of wip/rvcov-c)"
( cd "$A" && bash "$NOTES" put review "$COV_C_SHA" verdict=clean rounds=1 reviewer=codex writer=claude base="$COV_B_SHA" pieces=1 repository=readable >/dev/null \
  && bash "$NOTES" put gates "$COV_C_SHA" gates=pass selected=3 >/dev/null && bash "$NOTES" fetch )
( cd "$A" && bash "$NOTES" ready "$COV_C_SHA" ) && no "notes.sh ready accepted unreviewed intermediate B base" || ok "notes.sh ready refuses a note based at unreviewed intermediate commit B"
out="$(cd "$A" && LAND_REVIEW_CMD=true bash "$ROOT_DIR/scripts/land/ready.sh" wip/rvcov-c 2>&1)"; rc=$?
[ "$rc" != 0 ] && has "$out" "rejected by verify.sh" && ok "ready.sh refuses a note based at unreviewed intermediate commit B" || { no "ready.sh unreviewed B base: exit $rc"; echo "$out" | sed 's/^/      /'; }
# Default review command in ready.sh succeeds when LAND_REVIEW_CMD and LAND_REVIEW_WRITER are unset.
cat > "$TMP/fake-codex" <<'EOF'
#!/usr/bin/env bash
prompt="${!#}"
out=""
while [ $# -gt 0 ]; do
  case "$1" in -o) out="$2"; shift 2 ;; *) shift ;; esac
done
path="$(printf '%s\n' "$prompt" | sed -n 's/^diff --git a\/.* b\///p' | head -n 1)"
[ -n "$path" ] && [ -s "$path" ] || exit 9
head -n 1 "$path" >/dev/null || exit 9
case "$prompt" in *'b/scripts/review/__pycache__/planted.pyc'*|*'b/scripts/review/untracked-control.txt'*) echo 'untracked artifact entered review piece' >&2; exit 10 ;; esac
if [ "${PLANT_UNCITED:-0}" = 1 ]; then
  printf 'READ: %s\nREPOSITORY: readable\nVERDICT: clean\n' "$path" > "$out"
else
  printf '%s:1 Checked the supplied change.\nREAD: %s\nREPOSITORY: readable\nVERDICT: clean\n' "$path" "$path" > "$out"
fi
printf 'READ: required-checks.txt\nREPOSITORY: readable\nVERDICT: clean\ntokens used\n1,234\n'
EOF
chmod +x "$TMP/fake-codex"
( cd "$A" && git checkout -q -B wip/rv-default origin/main \
  && mkdir -p scripts/checks && ( ( cd "$ROOT_DIR" && git ls-files -z scripts/review scripts/checks/lib | tar --null -T - -cf - ) | tar -xf - ) \
  && git add -A && git commit -qm rv-default \
  && python3 -I "$ROOT_DIR/scripts/review/writer_provenance.py" stamp-commit --repo "$A" --model claude --round 1 --branch wip/rv-default --brief-sha256 2222222222222222222222222222222222222222222222222222222222222222 >/dev/null \
  && git push -q -f origin HEAD:refs/heads/wip/rv-default refs/notes/concrete-writer )
printf '__pycache__/\n*.pyc\n' >> "$A/.git/info/exclude"
( cd "$A" && mkdir -p scripts/review/__pycache__ && printf artifact > scripts/review/__pycache__/planted.pyc && printf artifact > scripts/review/untracked-control.txt )
ignored="$(git -C "$A" check-ignore scripts/review/__pycache__/planted.pyc)"
[ "$ignored" = scripts/review/__pycache__/planted.pyc ] && ok "planted Python cache is ignored" || no "Python cache control is not ignored: $ignored"
if git -C "$A" diff --name-only "$BASE" HEAD | grep -F '__pycache__/' >/dev/null; then no "fixture committed Python build artifacts"; else ok "fixture diff excludes working-tree Python artifacts"; fi
out="$(cd "$A" && unset LAND_REVIEW_CMD LAND_REVIEW_WRITER && PLANT_UNCITED=1 REVIEW_CODEX_BIN="$TMP/fake-codex" bash "$ROOT_DIR/scripts/land/ready.sh" wip/rv-default 2>&1)"; rc=$?
[ "$rc" != 0 ] && has "$out" "invalid reviewer evidence: citation: positive verdict requires at least one resolvable file:line citation" && has "$out" "gates=fail review=blocked" \
  && ok "uncited positive verdict blocks readiness with the citation diagnostic" || { no "uncited positive readiness: exit $rc"; echo "$out"; }
# Remove only the fixture's blocked note so the honest control starts at round one.
( cd "$A" && git notes --ref=refs/notes/concrete-review remove HEAD >/dev/null 2>&1 )
out="$(cd "$A" && unset LAND_REVIEW_CMD LAND_REVIEW_WRITER && REVIEW_CODEX_BIN="$TMP/fake-codex" bash "$ROOT_DIR/scripts/land/ready.sh" wip/rv-default 2>&1)"; rc=$?
[ "$rc" = 0 ] && has "$out" "gates=pass review=clean" \
  && ok "ready.sh default review command succeeds with LAND_REVIEW_WRITER unset and excludes untracked artifacts from pieces" || { no "ready.sh default review command: exit $rc"; echo "$out" | sed 's/^/      /'; }
for b in wip/rvcov-b wip/rvcov-c wip/rv-default; do git --git-dir="$TMP/origin.git" update-ref -d "refs/heads/$b" 2>/dev/null || true; done

echo "=== bisect --ci: a red CI run names the culprit, each probe its own run ==="
mk wip/cired cired CIRED red; mark wip/cired
mk wip/g3 g3 src/g3.txt g3; mark wip/g3
MAIN0="$(git --git-dir="$TMP/origin.git" rev-parse refs/heads/main)"
( cd "$W" && git fetch -q origin && git checkout -q --detach origin/main \
  && bash "$ROOT_DIR/scripts/land/train.sh" build bt --no-fetch origin/wip/cired origin/wip/g3 >/dev/null 2>&1 ) || no "build bt"
n0="$(cat "$TMP/gh/counter" 2>/dev/null || echo 0)"
out="$(cd "$W" && bash "$ROOT_DIR/scripts/land/train.sh" bisect bt --ci 2>&1)"; rc=$?
n1="$(cat "$TMP/gh/counter" 2>/dev/null || echo 0)"
runs=""; i=$((n0+1)); while [ "$i" -le "$n1" ]; do runs="$runs $(cat "$TMP/gh/run.$i")"; i=$((i+1)); done
# cired is the first half: a CI that never reports red would send the halving to g3 instead, and
# "alone it is red" is the culprit probe's own run.
[ "$rc" = 0 ] && has "$out" "step 1: first half [origin/wip/cired ] -> red" && has "$out" "culprit: origin/wip/cired" \
  && has "$out" "alone it is red" && has "$out" "build bt --force origin/wip/g3" \
  && ok "bisect --ci attributes the CI-red member and prints the rebuild without it" || { no "bisect --ci: exit $rc"; echo "$out" | sed 's/^/      /'; }
[ $((n1-n0)) -ge 2 ] && [ "$(printf '%s\n' $runs | sort -u | grep -c .)" = $((n1-n0)) ] \
  && ok "each probe dispatched its own CI run at its own SHA ($((n1-n0)) runs)" || no "bisect --ci runs: '$runs'"
[ "$(git --git-dir="$TMP/origin.git" rev-parse refs/heads/main)" = "$MAIN0" ] && ok "bisect --ci lands nothing" || no "bisect --ci moved main"
( cd "$W" && git checkout -q --detach origin/main )

echo "=== main is never force-pushed ==="
bad="$(grep -nE 'git push[^#]*(--force|[[:space:]]-f[[:space:]])[^#]*refs/heads/main' "$ROOT_DIR"/scripts/land/*.sh || true)"
[ -z "$bad" ] && ok "no forced push of refs/heads/main in scripts/land" || { no "forced push of main:"; echo "$bad" | sed 's/^/      /'; }

echo
echo "check_landing_ready: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
