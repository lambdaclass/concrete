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
mkdir -p "$TMP/nohooks" "$TMP/gh"
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

for r in origin fork; do git init -q --bare "$TMP/$r.git"; done
A="$TMP/agent"; W="$TMP/lander"
git init -q "$A"; cd "$A" || exit 2
for r in origin fork; do git remote add "$r" "$TMP/$r.git"; done
mkdir -p src
printf 'Build\n' > required-checks.txt  # train.sh refuses a landing with no required-checks.txt
git add -A && git commit -qm base
for r in origin fork; do git push -q "$r" main; done
BASE="$(git rev-parse HEAD)"
git clone -q "$TMP/origin.git" "$W"
( cd "$W" && git remote add fork "$TMP/fork.git" )

# mk <branch> <commit-message> <file> <content> [from]: one commit on <from> (default main), pushed.
mk(){ ( cd "$A" && git fetch -q origin && git checkout -q -B "$1" "${5:-origin/main}" && mkdir -p "$(dirname "$3")" \
        && printf '%s\n' "$4" > "$3" && git add -A && git commit -qm "$2" && git push -q -f origin "HEAD:refs/heads/$1" ); }
sha_of(){ git --git-dir="$TMP/origin.git" rev-parse "refs/heads/$1"; }
mark(){ # branch [review-verdict] [gates] [extra review key=value]
  local s; s="$(sha_of "$1")"
  ( cd "$A" && git fetch -q origin \
    && bash "$NOTES" put review "$s" verdict="${2:-clean}" rounds=1 reviewer=codex base="$(git -C "$W" rev-parse origin/main)" pieces=1 ${4:+"$4"} >/dev/null \
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

echo "=== ISS-0019: readiness checks schema and range ==="
for mode in intermediate self malformed; do
  mk "wip/range-$mode" hidden "src/hidden-$mode" hidden
  MID="$(sha_of "wip/range-$mode")"
  mk "wip/range-$mode" visible "src/visible-$mode" visible "$MID"
  RH="$(sha_of "wip/range-$mode")"
  nb="$MID"; [ "$mode" != self ] || nb="$RH"
  (cd "$A" && bash "$NOTES" put review "$RH" verdict=clean rounds=1 reviewer=codex base="$nb" pieces=1 >/dev/null && bash "$NOTES" put gates "$RH" gates=pass >/dev/null)
  if [ "$mode" = malformed ]; then
    (cd "$A" && bash "$NOTES" put review "$RH" verdict=clean >/dev/null)
  fi
  out="$(cd "$A" && bash "$NOTES" ready "$RH" 2>&1)"; rc=$?
  reason='review range does not cover required base'; [ "$mode" != malformed ] || reason='malformed review note'
  [ "$rc" = 2 ] && has "$out" "$reason" && ok "notes ready rejects $mode evidence for $reason" || { no "notes ready $mode: $rc $out"; }
  out="$(cd "$A" && bash "$ROOT_DIR/scripts/land/ready.sh" --no-review --no-remote "wip/range-$mode" 2>&1)"; rc=$?
  [ "$rc" = 2 ] && has "$out" "$reason" && ok "ready --no-review rejects $mode evidence for $reason" || { no "ready $mode: $rc $out"; }
done

echo "=== ISS-0019: explicit bases and notes ref isolation ==="
mk wip/explicit-base explicit src/explicit explicit; mark wip/explicit-base
EH="$(sha_of wip/explicit-base)"
out="$(cd "$A" && bash "$NOTES" ready "$EH" '' 2>&1)"; rc=$?
[ "$rc" = 2 ] && has "$out" 'required base is not a commit' && ok "notes ready rejects explicit empty base" || no "empty notes base: $rc $out"
for arg in empty missing; do
  if [ "$arg" = empty ]; then
    out="$(cd "$A" && bash "$ROOT_DIR/scripts/land/ready.sh" --base '' --dry-run wip/explicit-base 2>&1)"; rc=$?
  else
    out="$(cd "$A" && bash "$ROOT_DIR/scripts/land/ready.sh" --base 2>&1)"; rc=$?
  fi
  [ "$rc" != 0 ] && has "$out" '--base needs a nonempty ref' && ok "ready rejects $arg base argument" || no "ready $arg base: $rc $out"
done
# Keep the published clean snapshot, but replace canonical local evidence with findings.
(cd "$A" && git notes --ref=concrete-review add -f -m "$(printf 'verdict: findings\nrounds: 2\nreviewer: codex\nutc: 2026-11-11T00:00:00Z\nbase: %s\npieces: 1' "$BASE")" "$EH")
out="$(cd "$A" && REVIEW_NOTES_REF=refs/notes/remote/concrete-review bash "$ROOT_DIR/scripts/review/verify.sh" --required-base "$BASE" "$EH" 2>&1)"; rc=$?
[ "$rc" = 1 ] && has "$out" 'findings (round 2' && ok "ambient notes ref cannot override canonical evidence" || no "ambient ref: $rc $out"
out="$(cd "$A" && bash "$NOTES" ready "$EH" "$BASE" 2>&1)"; rc=$?
[ "$rc" = 0 ] && has "$out" 'clean (round 1' && ok "readiness explicitly selects published snapshot" || no "published ref: $rc $out"

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
