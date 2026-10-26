#!/usr/bin/env bash
# THE LANDING SCRIPT RESOLVES ONLY WHAT IS MECHANICAL AND REFUSES EVERYTHING ELSE (R-0539).
#
# scripts/land/train.sh builds one integrated candidate from several branches and lands it. The
# hand-run version of that job, on 2026-10-22, made the mistakes this gate pins down: a rebase piped
# to `tail` kept going and a commit was made mid-rebase; union-merged ledgers kept stale entries
# (two OPEN corpus entries for fixed bugs); and candidates were pushed without checking that the CI
# result described them. Each leg below drives the real script against a THROWAWAY repository with
# bare-repo remotes and a fake `gh`, so nothing here can touch this repository, its remotes or CI.
#
#   resolution  generated views (a ROADMAP generated block, a whole generated file) are regenerated,
#               ledgers are union-merged; the train carries both members' entries
#   refusal     a conflict in source or in prose outside a generated block aborts the merge (no
#               MERGE_HEAD left, exit 3); a union that duplicates a corpus key is refused; a
#               merge or rebase already in progress, a dirty tree and an existing train are refused
#   pipeline    check feeds the hook exactly base..head on ci/<name>; ci pushes and records the run
#   watch       a campaign-only failure is reported as such; a landing failure fetches log tails
#   land        refused on a head-SHA mismatch, a red landing job, a moved main, a remote named fork;
#               dry-run pushes nothing; a real land puts both remotes at the train head and cancels
#               main's duplicate push run (never the validating run)
#   dropped     (2026-10-25) a push that fails before the remote moved is retried; one that fails
#               after it moved is accepted from ls-remote; exhausted retries fail and move nothing
#   bisect      halving finds a seeded red member
#   report      the median time to land is computed from the log
set -uo pipefail
source "$(dirname "${BASH_SOURCE[0]}")/lib/gate_env.sh" || exit 2
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
TRAIN="$ROOT_DIR/scripts/land/train.sh"
REPORT="$ROOT_DIR/scripts/land/report.sh"
[ -f "$TRAIN" ] && [ -f "$REPORT" ] || { echo "FATAL: scripts/land/train.sh or report.sh missing" >&2; exit 2; }
TMP="$(mktemp -d)" || { echo "FATAL: could not create a sandbox directory" >&2; exit 2; }
[ -n "$TMP" ] && [ -d "$TMP" ] || { echo "FATAL: sandbox directory is not usable" >&2; exit 2; }
trap 'rm -rf "$TMP"' EXIT
PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }

# Isolation: no inherited repository (the hook leaks GIT_DIR), no user or system git config (hooks
# path, conflict style, signing), no toolchain re-entry.
unset GIT_DIR GIT_WORK_TREE GIT_INDEX_FILE GIT_COMMON_DIR GIT_OBJECT_DIRECTORY
export GIT_CONFIG_GLOBAL="$TMP/gitconfig" GIT_CONFIG_NOSYSTEM=1
cat > "$GIT_CONFIG_GLOBAL" <<EOF
[user]
	name = landing gate
	email = gate@example.invalid
[init]
	defaultBranch = main
[core]
	hooksPath = $TMP/nohooks
[merge]
	conflictStyle = diff3
EOF
mkdir -p "$TMP/nohooks"
export LAND_NO_NIX=1 LAND_STATE_DIR="$TMP/state" LAND_GH="$TMP/fake-gh" LAND_REPO=test/repo
export LAND_PUSH_RETRY_SLEEP=0 LAND_DUP_POLL=1 LAND_DUP_POLL_SLEEP=0
export FAKE_GH_DIR="$TMP/gh"; mkdir -p "$FAKE_GH_DIR"

cat > "$TMP/fake-gh" <<'GH'
#!/usr/bin/env bash
# A fake gh: run state comes from $FAKE_GH_DIR, every call is appended to calls.log.
echo "$*" >> "$FAKE_GH_DIR/calls.log"
case "$1 $2" in
  "workflow run") exit 0 ;;
  "run list") case " $* " in
                *" --event push "*) cat "$FAKE_GH_DIR/push_list.json" 2>/dev/null || echo '[]' ;;
                *) cat "$FAKE_GH_DIR/list.json" ;;
              esac ;;
  "run cancel") exit 0 ;;
  "run view") cat "$FAKE_GH_DIR/run.json" ;;
  "api "*) echo "fake log tail for $2" ;;
  *) echo "fake gh: unexpected $*" >&2; exit 1 ;;
esac
GH
chmod +x "$TMP/fake-gh"

W="$TMP/work"
for r in origin lambdaclass fork; do git init -q --bare "$TMP/$r.git"; done
git init -q "$W"
cd "$W" || exit 2
for r in origin lambdaclass fork; do git remote add "$r" "$TMP/$r.git"; done

# --- the toy repository: a ledger, a corpus map, a generated block, a generated file ---------------
mkdir -p scripts/gen scripts/tests docs/project src Concrete
BEGIN='<!-- BEGIN GENERATED: candidate record (scripts/gen/candidate_table.py; edit docs/project/candidates.tsv) -->'
END='<!-- END GENERATED: candidate record -->'
printf '# Changelog\n\n- base entry\n' > CHANGELOG.md
cat > scripts/tests/audit_bug_corpus.sh <<'EOF'
#!/usr/bin/env bash
declare -A BUG_TEST_MAP=(
  [100]="regress_100.con"
  [110]="regress_110.con"
)
declare -A SKIP_BUGS=(
  [050]="OPEN -- base"
)
EOF
printf 'id\tnote\nc0\tbase\n' > docs/project/candidates.tsv
# A second generated block whose generator does not exist: nothing may resolve a conflict in it.
GHOST='<!-- BEGIN GENERATED: ghost view (scripts/gen/ghost.py; edit nothing) -->'
printf '# Roadmap\n\nprose line one\n\n%s\n%s\n\nprose line two\n\n%s\nghost\n%s\n' "$BEGIN" "$END" "$GHOST" '<!-- END GENERATED: ghost view -->' > ROADMAP.md
cat > scripts/gen/candidate_table.py <<'EOF'
import re
import sys
rows = [l.split("\t") for l in open("docs/project/candidates.tsv").read().splitlines()[1:] if l]
if any(r[0] == "BROKEN" for r in rows):
    sys.exit("candidate record: BROKEN row refused")
body = "\n".join(f"| {r[0]} | {r[1]} |" for r in sorted(rows))
s = open("ROADMAP.md").read()
b = s.index("<!-- BEGIN GENERATED: candidate record"); b = s.index("\n", b) + 1
e = s.index("<!-- END GENERATED: candidate record -->")
open("ROADMAP.md", "w").write(s[:b] + body + "\n" + s[e:])
EOF
cat > scripts/gen/build_identity.sh <<'EOF'
#!/usr/bin/env bash
set -euo pipefail
printf 'def digest := "%s"\n' "$(cat src/*.txt | git hash-object --stdin)" > Concrete/BuildIdentity.lean
EOF
printf 'one\n' > src/one.txt; printf 'two\n' > src/two.txt
regen(){ python3 -I scripts/gen/candidate_table.py && bash scripts/gen/build_identity.sh; }
regen
git add -A && git commit -qm base
git push -q origin main && git push -q lambdaclass main && git push -q fork main
BASE="$(git rev-parse HEAD)"

branch(){ git checkout -q -b "$1" "$BASE"; }
commit_all(){ regen; git add -A; git commit -qm "$1"; }
corpus_add(){ python3 -I - "$1" <<'EOF'
import sys
p = "scripts/tests/audit_bug_corpus.sh"; s = open(p).read()
s = s.replace('  [100]="regress_100.con"\n', '  [100]="regress_100.con"\n' + sys.argv[1] + "\n", 1)
open(p, "w").write(s)
EOF
}
# b1, b2: ledger, corpus, generated-block and generated-file conflicts with each other only.
changelog_add(){ python3 -I -c 'import sys; p="CHANGELOG.md"; s=open(p).read(); open(p,"w").write(s.replace("- base entry", sys.argv[1] + "\n- base entry", 1))' "$1"; }
branch b1; changelog_add '- b1 entry'
corpus_add '  [201]="regress_201.con"'; printf 'c1\tfrom b1\n' >> docs/project/candidates.tsv
printf 'one b1\n' > src/one.txt; commit_all b1
branch b2; changelog_add '- b2 entry'
corpus_add '  [202]="regress_202.con"'; printf 'c2\tfrom b2\n' >> docs/project/candidates.tsv
printf 'two b2\n' > src/two.txt; commit_all b2
# b3: a source conflict with b1. b4/b5: a prose conflict outside the generated block.
branch b3; printf 'one b3\n' > src/one.txt; commit_all b3
branch b4; sed 's/prose line two/prose line two (b4)/' ROADMAP.md > r && mv r ROADMAP.md; commit_all b4
branch b5; sed 's/prose line two/prose line two (b5)/' ROADMAP.md > r && mv r ROADMAP.md; commit_all b5
corpus_after(){ python3 -I - "$1" "$2" <<'EOF'
import sys
p = "scripts/tests/audit_bug_corpus.sh"; s = open(p).read()
anchor = sys.argv[1] + "\n"
s = s.replace(anchor, anchor + sys.argv[2] + "\n", 1)
open(p, "w").write(s)
EOF
}
corpus_edit(){ sed "s/$1/$2/" scripts/tests/audit_bug_corpus.sh > a && mv a scripts/tests/audit_bug_corpus.sh; }
# b6/b7: both add [150], at different places: a clean merge that duplicates a key (bash keeps one).
branch b6; corpus_after '  [100]="regress_100.con"' '  [150]="regress_150a.con"'; commit_all b6
branch b7; corpus_after '  [110]="regress_110.con"' '  [150]="regress_150b.con"'; commit_all b7
# b10/b11: both re-key [100]: an edit conflict, never a union.
branch b10; corpus_edit 'regress_100.con' 'regress_100b.con'; commit_all b10
branch b11; corpus_edit 'regress_100.con' 'regress_100c.con'; commit_all b11
# b12/b13: both insert code after the maps: never unioned in a map-shaped ledger.
branch b12; echo 'echo twelve' >> scripts/tests/audit_bug_corpus.sh; commit_all b12
branch b13; echo 'echo thirteen' >> scripts/tests/audit_bug_corpus.sh; commit_all b13
# b17/b18: both insert inside a map, one line smuggling code after a well-formed key.
branch b17; corpus_after '  [100]="regress_100.con"' '  [170]="x" ); echo smuggled; BUG_TEST_MAP+=('; commit_all b17
branch b18; corpus_after '  [100]="regress_100.con"' '  [180]="regress_180.con"'; commit_all b18
# b14/b15: both edit the ghost block, whose generator does not exist.
branch b14; sed 's/^ghost$/ghost 14/' ROADMAP.md > r && mv r ROADMAP.md; commit_all b14
branch b15; sed 's/^ghost$/ghost 15/' ROADMAP.md > r && mv r ROADMAP.md; commit_all b15
# b16: a record the generator refuses (committed without regenerating, as a broken branch would be).
branch b16; printf 'BROKEN\tx\n' >> docs/project/candidates.tsv; git add -A; git commit -qm b16
# b8: the seeded red member for bisection (adds a file the "test" forbids).
branch b8; printf 'bad\n' > BAD; commit_all b8
branch b9; printf 'nine\n' > nine.txt; commit_all b9
branch b19; printf 'nineteen\n' > nineteen.txt; commit_all b19
git checkout -q main

no_op_left(){ for p in MERGE_HEAD CHERRY_PICK_HEAD REVERT_HEAD rebase-merge rebase-apply; do [ -e "$(git rev-parse --git-path "$p")" ] && return 1; done; return 0; }
show(){ git show "train/$1:$2" 2>/dev/null; }
has(){ case "$1" in *"$2"*) return 0 ;; esac; return 1; }

echo "=== resolution: generated views regenerated, ledgers union-merged ==="
out="$(bash "$TRAIN" build t1 --no-fetch b1 b2 2>&1)"; rc=$?
[ "$rc" = 0 ] && ok "build t1 (b1 b2) succeeds despite four conflicting files" || { no "build t1 exit $rc"; echo "$out" | sed 's/^/      /'; }
no_op_left && ok "no merge left in progress" || no "a merge is still in progress after build t1"
# Not vacuous: each of the four files really conflicted and took the intended route.
has "$out" "resolved CHANGELOG.md: union" && has "$out" "resolved scripts/tests/audit_bug_corpus.sh: union" \
  && has "$out" "resolved ROADMAP.md: every hunk is inside a generated block" \
  && has "$out" "resolved Concrete/BuildIdentity.lean: generated whole" \
  && ok "the four conflicts were real and each took its route" || { no "resolution routes"; echo "$out" | sed 's/^/      /'; }
cl="$(show t1 CHANGELOG.md)"
has "$cl" "- b1 entry" && has "$cl" "- b2 entry" && ok "CHANGELOG carries both members' entries" || no "CHANGELOG lost an entry"
corp="$(show t1 scripts/tests/audit_bug_corpus.sh)"
has "$corp" "[201]=" && has "$corp" "[202]=" && ok "corpus map carries [201] and [202]" || no "corpus map lost a key"
rm_="$(show t1 ROADMAP.md)"
has "$rm_" "| c1 | from b1 |" && has "$rm_" "| c2 | from b2 |" && ok "generated block regenerated from both members' records" || no "generated block is not the union view"
want="$(printf 'def digest := "%s"' "$(printf 'one b1\ntwo b2\n' | git hash-object --stdin)")"
[ "$(show t1 Concrete/BuildIdentity.lean)" = "$want" ] && ok "BuildIdentity regenerated over the merged sources" || no "BuildIdentity is stale on the train"
if git grep -qE '^(<{7}|>{7}|={7})( |$)' train/t1 -- . ; then no "conflict markers committed on the train"; else ok "no conflict markers committed"; fi
has "$(git log --format=%s "$BASE..train/t1")" "train/t1: merge b2" && ok "merge commits name member and SHA" || no "merge commit message"
st="$(cat "$TMP/state/t1/train.env")"
has "$st" "head=$(git rev-parse train/t1)" && has "$st" "b1@$(git rev-parse b1)" && ok "state binds head and member SHAs" || no "state file: $st"
log="$(cat "$TMP/state/log/t1.tsv" 2>/dev/null)"
has "$log" "	build	" && has "$log" "	passed	" && ok "build row logged" || no "no build row in the log"

echo "=== refusal: real conflicts stop the merge and leave nothing in progress ==="
out="$(bash "$TRAIN" build t2 --no-fetch b1 b3 2>&1)"; rc=$?
[ "$rc" = 3 ] && has "$out" "REAL CONFLICT src/one.txt" && ok "a source conflict stops with exit 3 and names the file" || { no "source conflict: exit $rc"; echo "$out" | sed 's/^/      /'; }
no_op_left && ok "the stopped merge was aborted (no MERGE_HEAD)" || no "MERGE_HEAD left behind"
[ -z "$(git status --porcelain --untracked-files=no)" ] && ok "tree clean after the stop" || no "tree dirty after the stop"
[ "$(git rev-parse train/t2^2)" = "$(git rev-parse b1)" ] && ok "train/t2 holds the members merged before the conflict" || no "train/t2 head unexpected"
out="$(bash "$TRAIN" build t3 --no-fetch b4 b5 2>&1)"; rc=$?
[ "$rc" = 3 ] && has "$out" "outside every generated block" && ok "a prose conflict outside the generated block is refused" || { no "prose conflict: exit $rc"; echo "$out" | sed 's/^/      /'; }
no_op_left || no "MERGE_HEAD left behind after the prose conflict"
git checkout -q main
out="$(bash "$TRAIN" build t4 --no-fetch b6 b7 2>&1)"; rc=$?
[ "$rc" != 0 ] && has "$out" "duplicate key [150]" && ok "a merge that duplicates a corpus key is refused" || { no "duplicate corpus key accepted (exit $rc)"; echo "$out" | sed 's/^/      /'; }
no_op_left || no "operation left in progress after the duplicate-key refusal"
git checkout -q main
out="$(bash "$TRAIN" build t4 --force --no-fetch b10 b11 2>&1)"; rc=$?
[ "$rc" = 3 ] && has "$out" "edit the same existing lines" && ok "two edits of one ledger entry are a real conflict, not a union" || { no "ledger edit conflict: exit $rc"; echo "$out" | sed 's/^/      /'; }
out="$(bash "$TRAIN" build t4 --force --no-fetch b12 b13 2>&1)"; rc=$?
[ "$rc" = 3 ] && has "$out" "outside every declare -A map" && ok "code inserted outside the corpus maps is never unioned" || { no "corpus code union: exit $rc"; echo "$out" | sed 's/^/      /'; }
out="$(bash "$TRAIN" build t4 --force --no-fetch b17 b18 2>&1)"; rc=$?
[ "$rc" = 3 ] && has "$out" "not a map entry" && ok "a map line carrying code after its key is never unioned" || { no "smuggled code: exit $rc"; echo "$out" | sed 's/^/      /'; }
out="$(bash "$TRAIN" build t4 --force --no-fetch b14 b15 2>&1)"; rc=$?
[ "$rc" = 3 ] && has "$out" "which the train will not run" && ok "a generated block whose generator will not run is a real conflict" || { no "ghost block: exit $rc"; echo "$out" | sed 's/^/      /'; }
out="$(bash "$TRAIN" build t4 --force --no-fetch b16 2>&1)"; rc=$?
[ "$rc" != 0 ] && has "$out" "a generator refused" && [ -z "$(git status --porcelain --untracked-files=no)" ] \
  && ok "a generator's refusal fails the build and leaves the tree clean" || { no "generator refusal: exit $rc"; echo "$out" | sed 's/^/      /'; }
has "$(cat "$TMP/state/t4/train.env")" "head=" && no "a refused build recorded a head" || ok "a refused build records no head"
no_op_left || no "operation left in progress after the generator refusal"
git checkout -q main
out="$(bash "$TRAIN" build t1 --no-fetch b1 2>&1)"; rc=$?
[ "$rc" != 0 ] && has "$out" "exists" && ok "an existing train is not overwritten without --force" || no "existing train overwritten (exit $rc)"
out="$(bash "$TRAIN" build 'bad/name' --no-fetch b1 2>&1)"; rc=$?
[ "$rc" != 0 ] && ok "a train name with '/' is refused" || no "bad name accepted"
echo dirty >> CHANGELOG.md
out="$(bash "$TRAIN" build t5 --no-fetch b1 2>&1)"; rc=$?
[ "$rc" != 0 ] && has "$out" "uncommitted" && ok "a dirty tree is refused" || no "dirty tree accepted"
git checkout -q -- CHANGELOG.md
git checkout -q -b scratch b1; git merge -q b3 >/dev/null 2>&1
out="$(bash "$TRAIN" build t6 --no-fetch b2 2>&1)"; rc=$?
[ "$rc" != 0 ] && has "$out" "in progress (MERGE_HEAD" && ok "a merge already in progress is refused" || { no "in-progress merge not detected (exit $rc)"; echo "$out" | sed 's/^/      /'; }
git merge --abort; git checkout -q main
git checkout -q --detach b2; git rebase -q b3 >/dev/null 2>&1 || true
out="$(bash "$TRAIN" build t6 --no-fetch b2 2>&1)"; rc=$?
[ "$rc" != 0 ] && has "$out" "in progress" && ok "a rebase in progress is refused" || { no "rebase in progress not detected (exit $rc)"; echo "$out" | sed 's/^/      /'; }
git rebase --abort 2>/dev/null; git checkout -q -f main

echo "=== pipeline: check -> ci -> watch -> land, with a fake gh ==="
git checkout -q train/t1
HEAD1="$(git rev-parse HEAD)"
cat > "$TMP/hook" <<'EOF'
#!/usr/bin/env bash
cat > "$FAKE_GH_DIR/hook.stdin"
[ ! -f "$FAKE_GH_DIR/hook.fail" ]
EOF
export LAND_HOOK="$TMP/hook"
out="$(bash "$TRAIN" ci t1 2>&1)"; rc=$?
[ "$rc" != 0 ] && has "$out" "no passing check" && ok "ci refuses a train with no passing check" || no "ci ran without a check"
out="$(bash "$TRAIN" check t1 --no-remote 2>&1)"; rc=$?
[ "$rc" = 0 ] && [ "$(cat "$FAKE_GH_DIR/hook.stdin")" = "refs/heads/train/t1 $HEAD1 refs/heads/ci/t1 $BASE" ] \
  && ok "check feeds the hook exactly base..head on ci/t1" || { no "check: exit $rc, hook stdin '$(cat "$FAKE_GH_DIR/hook.stdin" 2>/dev/null)'"; echo "$out" | sed 's/^/      /'; }
touch "$FAKE_GH_DIR/hook.fail"
out="$(bash "$TRAIN" check t1 --no-remote 2>&1)"; rc=$?
[ "$rc" != 0 ] && ok "check fails when the hook's selection fails" || no "check passed over a failing hook"
out="$(bash "$TRAIN" ci t1 2>&1)"; rc=$?
[ "$rc" != 0 ] && has "$out" "no passing check" && ok "a failed re-check withdraws the earlier pass" || no "ci used a stale pass after a failed re-check"
rm -f "$FAKE_GH_DIR/hook.fail"
bash "$TRAIN" check t1 --no-remote >/dev/null 2>&1 || no "re-check after repair"
out="$(bash "$TRAIN" --dry-run ci t1 2>&1)"; rc=$?
[ "$rc" = 0 ] && has "$out" "would dispatch" && [ -z "$(git ls-remote origin refs/heads/ci/t1)" ] \
  && ok "dry-run ci prints the push and dispatch and does neither" || no "dry-run ci: exit $rc"
printf '[{"databaseId": 4242, "headSha": "%s", "createdAt": "9999-01-01T00:00:00Z"}]' "$HEAD1" > "$FAKE_GH_DIR/list.json"
out="$(bash "$TRAIN" ci t1 2>&1)"; rc=$?
[ "$rc" = 0 ] && [ "$(git ls-remote origin refs/heads/ci/t1 | cut -f1)" = "$HEAD1" ] && has "$(cat "$TMP/state/t1/train.env")" "run_id=4242" \
  && has "$(cat "$FAKE_GH_DIR/calls.log")" "workflow run CI -R test/repo --ref ci/t1" \
  && ok "ci pushes ci/t1 at the head, dispatches, records run 4242" || { no "ci: exit $rc"; echo "$out" | sed 's/^/      /'; }

runjson(){ # sha landing-conclusion campaign-conclusion
  printf '{"status":"completed","conclusion":"%s","headSha":"%s","jobs":[{"name":"Build","conclusion":"%s","databaseId":11},{"name":"Gate mutation shard 3","conclusion":"%s","databaseId":12}]}' \
    "$2" "$1" "$2" "$3" > "$FAKE_GH_DIR/run.json"
}
runjson "$HEAD1" success failure
out="$(bash "$TRAIN" watch t1 --interval 1 2>&1)"; rc=$?
[ "$rc" = 0 ] && has "$out" "ONLY THE CAMPAIGN TIER FAILED" && has "$out" "fake log tail for repos/test/repo/actions/jobs/12/logs" \
  && ok "watch: a campaign-only failure is named, its log tail fetched, landing allowed" || { no "watch campaign-only: exit $rc"; echo "$out" | sed 's/^/      /'; }
runjson "$HEAD1" failure success
out="$(bash "$TRAIN" watch t1 --interval 1 2>&1)"; rc=$?
[ "$rc" = 1 ] && has "$out" "jobs/11/logs" && ok "watch: a landing failure is red and its log tail fetched" || no "watch landing failure: exit $rc"
runjson "$(git rev-parse b1)" success success
out="$(bash "$TRAIN" watch t1 --interval 1 2>&1)"; rc=$?
[ "$rc" = 1 ] && has "$out" "says nothing about this train" && ok "watch: a green run at another SHA is no verdict" || no "watch accepted a run at another SHA (exit $rc)"

runjson "$HEAD1" failure success
out="$(bash "$TRAIN" land t1 2>&1)"; rc=$?
[ "$rc" != 0 ] && has "$out" "did not succeed" && ok "land refuses a red landing job" || no "land accepted a red landing job (exit $rc)"
runjson "$(git rev-parse b1)" success success
out="$(bash "$TRAIN" land t1 2>&1)"; rc=$?
[ "$rc" != 0 ] && has "$out" "not the train HEAD" && ok "land refuses a run at another SHA" || no "land accepted a head-SHA mismatch (exit $rc)"
runjson "$HEAD1" success failure
# main's push run at the landed head (888) is the duplicate; the validating run (4242) is listed too
# and must never be cancelled.
printf '[{"databaseId": 888, "headSha": "%s", "status": "in_progress"}, {"databaseId": 4242, "headSha": "%s", "status": "in_progress"}]' "$HEAD1" "$HEAD1" > "$FAKE_GH_DIR/push_list.json"
out="$(LAND_REMOTE=fork bash "$TRAIN" land t1 2>&1)"; rc=$?
[ "$rc" != 0 ] && has "$out" "named 'fork'" && ok "land refuses the remote named fork as primary" || no "fork accepted as primary (exit $rc)"
out="$(LAND_MIRROR=fork bash "$TRAIN" land t1 2>&1)"; rc=$?
[ "$rc" != 0 ] && has "$out" "named 'fork'" && ok "land refuses the remote named fork as mirror" || no "fork accepted as mirror (exit $rc)"
out="$(LAND_MIRROR=nosuch bash "$TRAIN" land t1 2>&1)"; rc=$?
[ "$rc" != 0 ] && has "$out" "both remotes are required" && [ "$(git ls-remote origin refs/heads/main | cut -f1)" = "$BASE" ] \
  && ok "land refuses a missing mirror remote before pushing anything" || no "missing mirror remote: exit $rc"
[ "$(git ls-remote fork refs/heads/main | cut -f1)" = "$BASE" ] && ok "fork untouched" || no "fork was pushed"
out="$(bash "$TRAIN" --dry-run land t1 2>&1)"; rc=$?
[ "$rc" = 0 ] && has "$out" "would run" && [ "$(git ls-remote origin refs/heads/main | cut -f1)" = "$BASE" ] \
  && ok "dry-run land verifies and pushes nothing" || { no "dry-run land: exit $rc"; echo "$out" | sed 's/^/      /'; }

# main moves under the train: the result no longer describes what would land.
git clone -q "$TMP/origin.git" "$TMP/other"
( cd "$TMP/other" && echo moved > moved.txt && git add moved.txt && git commit -qm moved && git push -q origin main )
out="$(bash "$TRAIN" land t1 2>&1)"; rc=$?
[ "$rc" = 4 ] && has "$out" "moved from" && has "$out" "build t1 --force b1 b2" && ok "land refuses a moved main and prints the rebuild" || { no "moved main: exit $rc"; echo "$out" | sed 's/^/      /'; }
( cd "$TMP/other" && git push -q --force origin "$BASE:main" )
out="$(bash "$TRAIN" land t1 2>&1)"; rc=$?
[ "$rc" = 0 ] && [ "$(git ls-remote origin refs/heads/main | cut -f1)" = "$HEAD1" ] && [ "$(git ls-remote lambdaclass refs/heads/main | cut -f1)" = "$HEAD1" ] \
  && ok "land puts origin/main and lambdaclass/main at the train head" || { no "land: exit $rc"; echo "$out" | sed 's/^/      /'; }
[ "$(git ls-remote fork refs/heads/main | cut -f1)" = "$BASE" ] && ok "fork still untouched after landing" || no "fork was pushed"
[ ! -d "$(git rev-parse --git-common-dir)/concrete-push-both.lock" ] && ok "publish lock released" || no "publish lock left behind"
calls="$(cat "$FAKE_GH_DIR/calls.log")"
has "$calls" "run cancel 888 -R test/repo" && ! has "$calls" "run cancel 4242" && has "$out" "run 4242 validated that SHA" \
  && ok "land cancels main's duplicate push run and never the validating run" || { no "duplicate-run cancel"; echo "$out" | sed 's/^/      /'; }

echo "=== dropped: a push's result is read from the remote, and a failed push is retried ==="
# The pre-push hook stands in for GitHub dropping the connection: drop.always fails every push
# before the remote moves; drop.before fails one push before it moves; drop.after updates the
# remote itself and then fails, as a push does whose connection drops after the update.
cat > "$TMP/nohooks/pre-push" <<'EOF'
#!/usr/bin/env bash
# Like the real hook, a push that skips the gates (the ci base reset, the mirror) runs nothing.
[ "${CONCRETE_SKIP_GATES:-0}" = 1 ] && exit 0
[ -f "$FAKE_GH_DIR/drop.always" ] && exit 1
if [ -f "$FAKE_GH_DIR/drop.before" ]; then rm -f "$FAKE_GH_DIR/drop.before"; exit 1; fi
if [ -f "$FAKE_GH_DIR/drop.after" ]; then
  rm -f "$FAKE_GH_DIR/drop.after"
  read -r _lref lsha rref _rsha
  git -c core.hooksPath=/dev/null push -q "$1" "$lsha:$rref" || exit 3
  exit 1
fi
exit 0
EOF
chmod +x "$TMP/nohooks/pre-push"
git fetch -q origin
git checkout -q main
bash "$TRAIN" build t8 --no-fetch b19 >/dev/null 2>&1 || no "build t8"
git checkout -q train/t8
HEAD8="$(git rev-parse HEAD)"; MAIN8="$(git ls-remote origin refs/heads/main | cut -f1)"
bash "$TRAIN" check t8 --no-remote >/dev/null 2>&1 || no "check t8"
printf '[{"databaseId": 4343, "headSha": "%s", "createdAt": "9999-01-01T00:00:00Z"}]' "$HEAD8" > "$FAKE_GH_DIR/list.json"
touch "$FAKE_GH_DIR/drop.before"
out="$(bash "$TRAIN" ci t8 2>&1)"; rc=$?
[ "$rc" = 0 ] && has "$out" "retrying (1 of 3)" && [ "$(git ls-remote origin refs/heads/ci/t8 | cut -f1)" = "$HEAD8" ] \
  && ok "ci retries a push dropped before the remote moved" || { no "ci dropped push: exit $rc"; echo "$out" | sed 's/^/      /'; }
runjson "$HEAD8" success success
printf '[{"databaseId": 999, "headSha": "%s", "status": "queued"}]' "$HEAD8" > "$FAKE_GH_DIR/push_list.json"
touch "$FAKE_GH_DIR/drop.always"
out="$(bash "$TRAIN" land t8 --keep-main-ci 2>&1)"; rc=$?
rm -f "$FAKE_GH_DIR/drop.always"
[ "$rc" != 0 ] && has "$out" "failed 3 time(s)" && [ "$(git ls-remote origin refs/heads/main | cut -f1)" = "$MAIN8" ] \
  && [ ! -d "$(git rev-parse --git-common-dir)/concrete-push-both.lock" ] \
  && ok "exhausted retries fail the landing, move nothing and release the lock" || { no "exhausted retries: exit $rc"; echo "$out" | sed 's/^/      /'; }
touch "$FAKE_GH_DIR/drop.before" "$FAKE_GH_DIR/drop.after"
out="$(bash "$TRAIN" land t8 --keep-main-ci 2>&1)"; rc=$?
[ "$rc" = 0 ] && has "$out" "retrying (1 of 3)" && has "$out" "the connection dropped after the update" \
  && [ "$(git ls-remote origin refs/heads/main | cut -f1)" = "$HEAD8" ] && [ "$(git ls-remote lambdaclass refs/heads/main | cut -f1)" = "$HEAD8" ] \
  && ok "land retries a push dropped before the update, accepts one dropped after it, and both remotes reach the head" || { no "dropped land: exit $rc"; echo "$out" | sed 's/^/      /'; }
! has "$(cat "$FAKE_GH_DIR/calls.log")" "run cancel 999" && ok "--keep-main-ci leaves main's run alone" || no "--keep-main-ci cancelled a run"
[ ! -f "$FAKE_GH_DIR/drop.before" ] && [ ! -f "$FAKE_GH_DIR/drop.after" ] && ok "both dropped-connection seams fired" || no "a dropped-connection seam never fired (vacuous leg)"
rm -f "$TMP/nohooks/pre-push"

echo "=== bisect: halving finds the seeded red member ==="
git checkout -q main
bash "$TRAIN" build t7 --no-fetch b9 b8 b4 >/dev/null 2>&1 || no "build t7"
out="$(bash "$TRAIN" bisect t7 --test '! test -e BAD' 2>&1)"; rc=$?
[ "$rc" = 0 ] && has "$out" "culprit: b8" && has "$out" "build t7 --force b9 b4" && ok "bisect attributes b8 and prints the rebuild without it" || { no "bisect: exit $rc"; echo "$out" | sed 's/^/      /'; }
no_op_left && [ -z "$(git status --porcelain --untracked-files=no)" ] && ok "bisect leaves the checkout clean" || no "bisect left state behind"

echo "=== push-both.sh: both remotes required, refusals before anything is pushed ==="
cp "$ROOT_DIR/scripts/push-both.sh" scripts/push-both.sh
before="$(git ls-remote origin refs/heads/main | cut -f1)"
out="$(PRIMARY=fork bash scripts/push-both.sh 2>&1)"; rc=$?
[ "$rc" = 2 ] && has "$out" "remote named fork" && ok "push-both refuses PRIMARY=fork" || no "push-both PRIMARY=fork: exit $rc"
out="$(MIRROR=fork bash scripts/push-both.sh 2>&1)"; rc=$?
[ "$rc" = 2 ] && has "$out" "remote named fork" && ok "push-both refuses MIRROR=fork" || no "push-both MIRROR=fork: exit $rc"
out="$(MIRROR=nosuch bash scripts/push-both.sh 2>&1)"; rc=$?
[ "$rc" = 1 ] && has "$out" "both remotes are required" && ok "push-both refuses a missing mirror remote" || no "push-both missing mirror: exit $rc"
[ "$(git ls-remote origin refs/heads/main | cut -f1)" = "$before" ] && [ "$(git ls-remote fork refs/heads/main | cut -f1)" = "$BASE" ] \
  && ok "no refused push-both moved origin or fork" || no "a refused push-both pushed something"
rm -f scripts/push-both.sh

echo "=== report ==="
out="$(bash "$REPORT" 2>&1)"; rc=$?
[ "$rc" = 0 ] && has "$out" "median time to land" && has "$out" "landed: 2" && ok "report prints the median time to land" || { no "report: exit $rc"; echo "$out" | sed 's/^/      /'; }

echo
echo "check_landing_script: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
