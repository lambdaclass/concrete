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
#               no run (main's own run records the train's, R-0570)
#   record      (2026-11-06, R-0570) main's run names the run that qualified its exact SHA only when
#               the newest completed dispatch of the CI workflow at that SHA AND at that base (its
#               title, from the base input train.sh ci passes) passed every landing-tier and required
#               job and the head is ahead of the base; a missing run, a run at another SHA or base, of
#               another workflow or event, unfinished or excluded, a skipped,
#               cancelled or failed job, an older green run behind a newer red one, a diverged or
#               absent base: no record. The workflow gates every job on it (structure check)
#   dropped     (2026-10-25) a push that fails before the remote moved is retried; one that fails
#               after it moved is accepted from ls-remote; exhausted retries fail and move nothing
#   bisect      halving finds a seeded red member
#   report      the median time to land is computed from the log
#   gate host   (2026-11-05) check's impact leg on the gate host: bound to the train head's SHA (a
#               run at another SHA, an unreachable host or a lost log is INCOMPLETE and blocks; only
#               --local-gates runs here); the hook reuses the attestation only for this exact push, and
#               a tampered log or attestation is refused
#   review      (2026-11-06) review fails closed: check with no review command fails; a reviewer that
#               exits 0 but leaves no note, a malformed, blocked or findings note fails the leg; check
#               skips the reviewer when every member is reviewed clean; land requires a clean head
#               note, or (head unreviewed) a clean note on every member, and refuses an absent,
#               malformed, blocked or findings note in either place
#   required    (2026-11-04) a required job (required-checks.txt) that is missing, skipped or
#               cancelled refuses watch and land; a candidate drops a requirement main imposes only
#               through a transition row (replaced by jobs it requires and that pass, or retired with
#               a reason and reported as retired); the file names the workflow's landing-tier jobs
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
[merge]
	conflictStyle = diff3
EOF
mkdir -p "$TMP/nohooks"
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
export LAND_NO_NIX=1 LAND_STATE_DIR="$TMP/state" LAND_GH="$TMP/fake-gh" LAND_REPO=test/repo
export LAND_PUSH_RETRY_SLEEP=0
unset LAND_GATES_HOST LAND_GATES_TRANSPORT CONCRETE_PREPUSH_NO_REUSE
export FAKE_GH_DIR="$TMP/gh"; mkdir -p "$FAKE_GH_DIR"

cat > "$TMP/fake-gh" <<'GH'
#!/usr/bin/env bash
# A fake gh: run state comes from $FAKE_GH_DIR, every call is appended to calls.log.
echo "$*" >> "$FAKE_GH_DIR/calls.log"
case "$1 $2" in
  "workflow run") exit 0 ;;
  "run list") cat "$FAKE_GH_DIR/list.json" ;;
  "run cancel") exit 0 ;;
  "run view") if [ -f "$FAKE_GH_DIR/run.$3.json" ]; then cat "$FAKE_GH_DIR/run.$3.json"; else cat "$FAKE_GH_DIR/run.json"; fi ;;
  "api repos/test/repo/compare/"*) cat "$FAKE_GH_DIR/compare.json" 2>/dev/null || exit 1 ;;
  "api repos/test/repo/actions/runs?"*) cat "$FAKE_GH_DIR/runs.json" 2>/dev/null || exit 1 ;;
  "api "*) echo "fake log tail for $2" ;;
  *) echo "fake gh: unexpected $*" >&2; exit 1 ;;
esac
GH
chmod +x "$TMP/fake-gh"

# A review note as scripts/review/review.sh writes it (scripts/review/verify.sh's six keys), published
# to origin. put-review <sha> <clean|findings|blocked|malformed|none>[@rounds]; none removes the note.
cat > "$TMP/put-review" <<'EOF'
#!/usr/bin/env bash
set -e
sha="$(git rev-parse "$1")"; R=refs/notes/concrete-review
git fetch -q origin "+$R:$R" 2>/dev/null || true
case "$2" in
  none) git notes --ref=$R remove --ignore-missing "$sha" 2>/dev/null ;;
  malformed) git notes --ref=$R add -f -m "verdict: clean" "$sha" ;;
  *) git notes --ref=$R add -f -m "verdict: ${2%@*}
rounds: $(case "$2" in *@*) echo "${2#*@}" ;; *) echo 1 ;; esac)
reviewer: codex
utc: 2026-11-06T00:00:00Z
base: $sha
pieces: 1" "$sha" ;;
esac
git show-ref -q --verify $R || exit 0
CONCRETE_SKIP_GATES=1 git push -q origin $R
EOF
# The review command of every check (LAND_REVIEW_CMD): called with --base B --head H, it logs the
# call and answers per $FAKE_GH_DIR/review.mode: a note of that kind on H (default clean), none (exit
# 0, writes nothing) or fail (exit 1, writes nothing).
cat > "$TMP/fake-review" <<'EOF'
#!/usr/bin/env bash
mode="$(cat "$FAKE_GH_DIR/review.mode" 2>/dev/null || echo clean)"
echo "$4 $mode" >> "$FAKE_GH_DIR/review.log"
case "$mode" in none) exit 0 ;; fail) exit 1 ;; esac
bash "$(dirname "$0")/put-review" "$4" "$mode"
EOF
export LAND_REVIEW_CMD="bash $TMP/fake-review"

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
printf '# required\nBuild\n' > required-checks.txt
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

echo "=== gate host: check's impact leg on the gate host, bound to the SHA; INCOMPLETE blocks ==="
# A fake transport in place of gates_transport_ssh.sh. It reads the SHA from the bundle it was given
# (as run-at-ref.sh would), records what it was asked, and answers per $FAKE_GH_DIR/transport.mode.
cat > "$TMP/fake-transport" <<'EOF'
#!/usr/bin/env bash
host="$1" bundle="$2" ref="$3" cmd="$4"
printf '%s\n' "$host" > "$FAKE_GH_DIR/transport.host"
printf '%s\n' "$cmd" > "$FAKE_GH_DIR/transport.cmd"
sha="$(git bundle list-heads "$bundle" "$ref" | cut -d' ' -f1)"
printf '%s\n' "$sha" > "$FAKE_GH_DIR/transport.sha"
mode="$(cat "$FAKE_GH_DIR/transport.mode" 2>/dev/null || echo ok)"
case "$mode" in
  unreachable) echo "ssh: connect to host $host port 22: Connection refused" >&2; exit 255 ;;
  wrongsha) sha="$(cat "$FAKE_GH_DIR/transport.other")" ;;
esac
echo "[concrete-remote] ref=$ref sha=$sha clone=/runs/x host=fake-$host"
case "$mode" in
  fail) echo "FAIL some gate"; echo "[concrete-remote] exit=1 elapsed=1s sha=$sha"; exit 1 ;;
  lost) echo "gate output, then the connection drops"; exit 255 ;;
  norecord) echo "=== concrete-remote-record-none"; echo "[concrete-remote] exit=0 elapsed=1s sha=$sha"; exit 0 ;;
esac
echo "=== concrete-remote-record-begin"
printf 'format=concrete-prepush-record-v1\nhead=%s\npushed=%s\nstatus=pass\ncomplete=1\n' "$sha" "$sha"
echo "=== concrete-remote-record-end"
echo "[concrete-remote] exit=0 elapsed=1s sha=$sha"
EOF
chmod +x "$TMP/fake-transport"
export LAND_GATES_TRANSPORT="$TMP/fake-transport"
ATT="$(git rev-parse --git-path concrete-prepush-remote-record)"; case "$ATT" in /*) ;; *) ATT="$W/$ATT" ;; esac
gh_check(){ # mode -> runs check t1 against the gate host fakehost; output in $out, status in $rc
  printf '%s\n' "$1" > "$FAKE_GH_DIR/transport.mode"
  rm -f "$FAKE_GH_DIR/hook.stdin" "$FAKE_GH_DIR/transport.host" "$FAKE_GH_DIR/transport.cmd"
  out="$(LAND_GATES_HOST=fakehost CONCRETE_TEST_FWD='a b' CONCRETE_SKIP_GATES=1 bash "$TRAIN" check t1 --no-remote 2>&1)"; rc=$?
}
lastrow(){ tail -n 1 "$TMP/state/log/t1.tsv"; }
rowis(){ lastrow | awk -F'\t' -v s="$1" -v v="$2" '$3 == s && $8 == v { f = 1 } END { exit !f }'; } # stage result
gh_check ok
[ "$rc" = 0 ] && [ "$(cat "$FAKE_GH_DIR/transport.host" 2>/dev/null)" = fakehost ] && [ "$(cat "$FAKE_GH_DIR/transport.sha")" = "$HEAD1" ] \
  && ok "check runs the impact leg on the gate host, from a bundle holding exactly the train head" || { no "gate host ok: exit $rc"; echo "$out" | sed 's/^/      /'; }
[ ! -f "$FAKE_GH_DIR/hook.stdin" ] && ok "with a gate host the gates do not run on this machine" || no "the local hook ran although the gate host passed"
cmd_="$(cat "$FAKE_GH_DIR/transport.cmd" 2>/dev/null)"
has "$cmd_" "impact_gates.sh --base $BASE --head $HEAD1 --src-ref refs/heads/train/t1 --dst-ref refs/heads/ci/t1" \
  && ok "the host runs the hook's selection over exactly base..head on ci/t1" || no "remote command: $cmd_"
has "$cmd_" 'export CONCRETE_TEST_FWD=a\ b;' && ! has "$cmd_" 'export CONCRETE_SKIP_GATES' \
  && [ "${cmd_%%;*}" = "unset CONCRETE_SKIP_GATES CONCRETE_PREPUSH_REENTERED" ] \
  && ok "CONCRETE_* is forwarded quoted; the bypass is never forwarded and is cleared first" || no "forwarded env: $cmd_"
grep -qx "head=$HEAD1" "$ATT" 2>/dev/null && [ "$(tail -n 1 "$ATT")" = complete=1 ] && [ -f "$ATT.log" ] \
  && ok "the pass is attested for the train head, with the host's log kept" || no "no attestation for $HEAD1 at $ATT"
rowis check passed && has "$(lastrow)" "impact on fakehost" && has "$(cat "$TMP/state/t1/train.env")" "checked=$HEAD1" \
  && ok "the check row names the gate host; the train is checked at its head" || no "check row: $(lastrow)"
printf '%s\n' "$(git rev-parse b1)" > "$FAKE_GH_DIR/transport.other"
gh_check wrongsha
[ "$rc" != 0 ] && has "$out" "INCOMPLETE" && has "$out" "another commit" && [ ! -f "$ATT" ] \
  && ok "a run at another SHA is no result: INCOMPLETE, and the old attestation is withdrawn" || { no "wrong SHA: exit $rc"; echo "$out" | sed 's/^/      /'; }
rowis check incomplete && ! has "$(cat "$TMP/state/t1/train.env")" "checked=$HEAD1" \
  && ok "an incomplete check is logged as incomplete and withdraws the earlier pass" || no "incomplete row: $(lastrow)"
out2="$(bash "$TRAIN" ci t1 2>&1)"
has "$out2" "no passing check" && ok "ci refuses after an incomplete check" || no "ci ran after an incomplete check"
gh_check unreachable
[ "$rc" != 0 ] && has "$out" "INCOMPLETE" && has "$out" "--local-gates" && rowis check incomplete && [ ! -f "$FAKE_GH_DIR/hook.stdin" ] \
  && ok "an unreachable gate host is INCOMPLETE, blocks, and never falls back to a local run" || { no "unreachable: exit $rc"; echo "$out" | sed 's/^/      /'; }
gh_check lost
[ "$rc" != 0 ] && has "$out" "INCOMPLETE" && rowis check incomplete \
  && ok "a log without its exit line (lost connection) is INCOMPLETE" || { no "lost connection: exit $rc"; echo "$out" | sed 's/^/      /'; }
gh_check fail
[ "$rc" != 0 ] && has "$out" "FAILED on fakehost" && rowis check failed \
  && ok "gates that fail on the host at the head fail the check (not incomplete)" || { no "remote failure: exit $rc"; echo "$out" | sed 's/^/      /'; }
gh_check norecord
[ "$rc" = 0 ] && has "$out" "no attestation" && [ ! -f "$ATT" ] \
  && ok "a pass without a remote record passes, attests nothing, says the push runs the gates" || { no "norecord: exit $rc"; echo "$out" | sed 's/^/      /'; }
printf 'unreachable\n' > "$FAKE_GH_DIR/transport.mode"; rm -f "$FAKE_GH_DIR/hook.stdin" "$FAKE_GH_DIR/transport.host"
out="$(LAND_GATES_HOST=fakehost bash "$TRAIN" check t1 --no-remote --local-gates 2>&1)"; rc=$?
[ "$rc" = 0 ] && [ -f "$FAKE_GH_DIR/hook.stdin" ] && [ ! -f "$FAKE_GH_DIR/transport.host" ] && has "$(lastrow)" "impact here" \
  && ok "--local-gates is the explicit opt-in to run the gates on this machine" || { no "--local-gates: exit $rc"; echo "$out" | sed 's/^/      /'; }
printf 'ok\n' > "$FAKE_GH_DIR/transport.mode"; rm -f "$FAKE_GH_DIR/transport.host"
git config land.gatesHost cfghost
out="$(bash "$TRAIN" check t1 --no-remote 2>&1)"; rc=$?
[ "$rc" = 0 ] && [ "$(cat "$FAKE_GH_DIR/transport.host" 2>/dev/null)" = cfghost ] && ok "git config land.gatesHost names the gate host" || no "land.gatesHost: exit $rc"
out="$(bash "$TRAIN" --dry-run check t1 --no-remote 2>&1)"
has "$out" "impact gates on cfghost" && ok "dry-run check names the gate host" || no "dry-run: $out"
rm -f "$FAKE_GH_DIR/hook.stdin" "$FAKE_GH_DIR/transport.host"
out="$(LAND_GATES_HOST= bash "$TRAIN" check t1 --no-remote 2>&1)"; rc=$?
[ "$rc" = 0 ] && [ -f "$FAKE_GH_DIR/hook.stdin" ] && [ ! -f "$FAKE_GH_DIR/transport.host" ] \
  && ok "an empty LAND_GATES_HOST overrides the config (no gate host)" || no "empty LAND_GATES_HOST: exit $rc"
git config --unset land.gatesHost
unset LAND_GATES_TRANSPORT

# run-at-ref.sh fetches the train head from a bundle into a gate clone that has only main.
git clone -q --bare "$TMP/origin.git" "$TMP/gateclone.git"
git update-ref refs/land-remote/test "$HEAD1"
git bundle create "$TMP/cand.bundle" refs/land-remote/test "^$BASE" >/dev/null 2>&1
git update-ref -d refs/land-remote/test
out="$(RUN_AT_REF_REPO="$TMP/gateclone.git" RUN_AT_REF_RUNS="$TMP/runs" RUN_AT_REF_SOURCE="$TMP/cand.bundle" RUN_AT_REF_NO_NIX=1 \
  bash "$ROOT_DIR/scripts/land/run-at-ref.sh" refs/land-remote/test 'echo at=$(git rev-parse HEAD)' 2>&1)"; rc=$?
[ "$rc" = 0 ] && has "$out" "sha=$HEAD1 clone=" && has "$out" "at=$HEAD1" && has "$out" "exit=0 " \
  && ok "run-at-ref.sh runs exactly the bundled head and prints sha= on both lines" || { no "run-at-ref from a bundle: exit $rc"; echo "$out" | sed 's/^/      /'; }

echo "=== gate host: the pushing hook reuses the attestation only for this exact push ==="
# The library the hook calls, driven directly in its own repository (the real hook needs the gate runner).
R="$TMP/rr"; git init -q "$R"
( cd "$R" && mkdir scripts && echo 'leanprover/lean4:v0' > lean-toolchain && echo 'echo gate' > scripts/g.sh \
  && git add -A && git commit -qm one && echo 'echo gate2' > scripts/g2.sh && git add -A && git commit -qm two )
R1="$(git -C "$R" rev-parse HEAD~1)" R2="$(git -C "$R" rev-parse HEAD)"
printf 'bash scripts/g.sh\n' > "$TMP/plan"; printf 'scripts/g2.sh\n' > "$TMP/changed"
lib(){ ( unset CONCRETE_TEST_FWD; cd "$R" && source "$ROOT_DIR/scripts/checks/lib/prepush_record.sh" && "$@" ); }
mklog(){ # sha exit-rc extra-record-lines... -> $TMP/rlog: a run-at-ref log carrying a record of this push
  local sha="$1" erc="$2"; shift 2
  lib ppr_remote_components "$R" "$sha" "$TMP/changed" "$TMP/plan" "$TMP/rcomp" || return 1
  { echo "[concrete-remote] ref=refs/land-remote/x sha=$sha clone=/runs/x host=gatehost"
    echo "gate output"
    echo "=== concrete-remote-record-begin"
    echo "format=concrete-prepush-record-v1"; cat "$TMP/rcomp"; echo "binary=hostbinary"
    for l in "$@"; do echo "$l"; done
    echo "complete=1"
    echo "=== concrete-remote-record-end"
    echo "[concrete-remote] exit=$erc elapsed=9s sha=$sha"; } > "$TMP/rlog"
}
reuse(){ # sha [changed-file] [plan-file] -> the hook's decision for a push of sha; PPR_REASON on stdout
  ( unset CONCRETE_TEST_FWD; cd "$R" && source "$ROOT_DIR/scripts/checks/lib/prepush_record.sh" \
    && ppr_remote_components "$R" "$1" "${2:-$TMP/changed}" "${3:-$TMP/plan}" "$TMP/pcomp" \
    && ppr_remote_check "$R" "$TMP/pcomp" && echo REUSE || echo "refused:$PPR_REASON" )
}
RATT="$(lib ppr_remote_path "$R")"
mklog "$R2" 0 status=pass && lib ppr_remote_attest "$R" "$TMP/rlog" gatehost "$R2" \
  && [ "$(reuse "$R2")" = REUSE ] && ok "the hook reuses an attested pass for exactly this push" || no "exact push not reused: $(reuse "$R2")"
git -C "$R" checkout -q "$R1"
has "$(reuse "$R1")" "refused:attestation_other_head" && ok "a push of another SHA does not reuse it" || no "other SHA: $(reuse "$R1")"
git -C "$R" checkout -q -
has "$(reuse "$R1")" "refused:pushed_sha_not_head" && ok "a push of a ref that is not HEAD does not reuse it" || no "non-HEAD push: $(reuse "$R1")"
printf 'scripts/g2.sh\nother\n' > "$TMP/changed2"
has "$(reuse "$R2" "$TMP/changed2")" "refused:changed:base" && ok "another changed-file list does not reuse it" || no "changed list: $(reuse "$R2" "$TMP/changed2")"
printf 'bash scripts/g.sh\nbash scripts/g2.sh\n' > "$TMP/plan2"
has "$(reuse "$R2" "$TMP/changed" "$TMP/plan2")" "refused:changed:selection" && ok "another selection does not reuse it" || no "selection"
has "$( ( export CONCRETE_EXTRA=1; reuse "$R2" ) )" "refused:changed:env" && ok "another CONCRETE_* environment does not reuse it" || no "env"
echo dirty >> "$R/scripts/g.sh"
has "$(reuse "$R2")" "refused:tracked_tree_dirty" && ok "a dirty tracked tree does not reuse it" || no "dirty tree"
git -C "$R" checkout -q -- scripts/g.sh
cp "$RATT.log" "$TMP/goodlog"; cp "$RATT" "$TMP/goodatt"
echo "[concrete-remote] note" >> "$RATT.log"
has "$(reuse "$R2")" "refused:attested_log_changed" && ok "a tampered log is refused (digest)" || no "tampered log: $(reuse "$R2")"
cp "$TMP/goodlog" "$RATT.log"
sed "s/^selection=.*/selection=0000/" "$TMP/goodlog" > "$RATT.log"
sed "s/^log_digest=.*/log_digest=$(lib ts_digest < "$RATT.log")/" "$TMP/goodatt" > "$RATT"
has "$(reuse "$R2")" "refused:changed:selection" && ok "a tampered log re-digested into the attestation is still refused (components)" || no "re-digested tamper: $(reuse "$R2")"
cp "$TMP/goodlog" "$RATT.log"; sed "s/^head=.*/head=$R1/" "$TMP/goodatt" > "$RATT"
has "$(reuse "$R2")" "refused:attestation_other_head" && ok "a tampered attestation head is refused" || no "tampered attestation: $(reuse "$R2")"
sed '$d' "$TMP/goodatt" > "$RATT"
has "$(reuse "$R2")" "refused:attestation_incomplete" && ok "a truncated attestation is refused" || no "truncated attestation"
cp "$TMP/goodatt" "$RATT"; [ "$(reuse "$R2")" = REUSE ] || no "restored attestation not reused"
mklog "$R2" 1 status=pass; lib ppr_remote_attest "$R" "$TMP/rlog" gatehost "$R2"; arc=$?
[ "$arc" != 0 ] && [ ! -f "$RATT" ] && ok "a log whose run exited non-zero is never attested" || no "exit=1 attested"
mklog "$R2" 0 status=fail; lib ppr_remote_attest "$R" "$TMP/rlog" gatehost "$R2"; arc=$?
[ "$arc" != 0 ] && [ ! -f "$RATT" ] && ok "a remote record that is not a pass is never attested" || no "status=fail attested"
mklog "$R2" 0 status=pass; echo "[concrete-remote] exit=0 elapsed=1s sha=$R2" >> "$TMP/rlog"
lib ppr_remote_attest "$R" "$TMP/rlog" gatehost "$R2"; arc=$?
[ "$arc" != 0 ] && ok "a log with two exit lines is never attested" || no "duplicate exit line attested"
mklog "$R2" 0 status=pass; lib ppr_remote_attest "$R" "$TMP/rlog" gatehost "$R1"; arc=$?
[ "$arc" != 0 ] && ok "a log is never attested for a SHA it does not name" || no "attested for another SHA"
mklog "$R2" 0 status=pass-deferred "deferred_gate=bash scripts/g.sh" && lib ppr_remote_attest "$R" "$TMP/rlog" gatehost "$R2" \
  && [ "$(reuse "$R2")" = REUSE ] && [ "$(lib ppr_remote_deferred "$R")" = "bash scripts/g.sh" ] \
  && ok "a pass-deferred remote run is reused and its deferrals reported" || no "pass-deferred"
d1="$(lib _ppr_env_digest)"; d2="$( export CONCRETE_ML=$'one\ntwo'; lib _ppr_env_digest )"; d3="$( export CONCRETE_ML=$'one\nthree'; lib _ppr_env_digest )"
[ -n "$d1" ] && [ "$d1" != "$d2" ] && [ "$d2" != "$d3" ] && ok "the environment digest binds whole values (a changed continuation line differs)" || no "multiline env digest: $d1 $d2 $d3"
has "$( ( export CONCRETE_PREPUSH_REENTERED=1; reuse "$R2" ) )" "REUSE" \
  && ok "the hook's nix re-entry bookkeeping does not break reuse (push from outside the gate shell)" || no "REENTERED breaks reuse"

echo "=== gate host end to end: the real ssh transport, shimmed, carries the command intact ==="
# The real gates_remote.sh -> gates_transport_ssh.sh -> run-at-ref.sh chain, with ssh and scp
# replaced by local shims (the "host" is a directory) and a committed stub impact_gates.sh that
# records its environment and writes the hook's record the way the real hook would, unless the
# bypass is set (then it "skips", exit 0, no record: what the real hook does).
RI="$TMP/ri"; HH="$TMP/gatehost"; mkdir -p "$HH"
git init -q --bare "$TMP/ri-origin.git"; git init -q "$RI"
( cd "$RI" && git remote add origin "$TMP/ri-origin.git" && mkdir -p scripts/land \
  && echo 'leanprover/lean4:v0' > lean-toolchain && echo 'echo gate' > scripts/g.sh \
  && cat > scripts/land/impact_gates.sh <<'EOF'
#!/usr/bin/env bash
# stub: --base B --head H --src-ref S --dst-ref D
env -0 > "$CONCRETE_TEST_OUT/remote.env"
printf '%s\n' "$@" > "$CONCRETE_TEST_OUT/remote.args"
[ -z "${CONCRETE_SKIP_GATES:-}" ] || { echo "pre-push: gates SKIPPED"; exit 0; }
source "$CONCRETE_TEST_LIB"
t="$(mktemp -d)"
git diff --name-only "$2" "$4" > "$t/changed"
printf 'bash scripts/g.sh\n' > "$t/plan"
ppr_remote_components "$PWD" "$4" "$t/changed" "$t/plan" "$t/comp" || { echo "stub: $PPR_REASON"; exit 1; }
{ echo "format=concrete-prepush-record-v1"; cat "$t/comp"; echo status=pass; echo complete=1; } > "$(git rev-parse --git-path concrete-prepush-record)"
rm -rf "$t"
echo "impact: stub gates passed"
EOF
  git add -A && git commit -qm base && git push -q origin HEAD:main \
  && echo 'echo gate2' > scripts/g2.sh && git add -A && git commit -qm head )
RIB="$(git -C "$RI" rev-parse HEAD~1)" RIH="$(git -C "$RI" rev-parse HEAD)"
git clone -q "$TMP/ri-origin.git" "$HH/concrete2"
cat > "$TMP/scp-shim" <<'EOF'
#!/usr/bin/env bash
# scp -r <dir> host:<name>  ->  copy into the fake host's home
src="${@: -2:1}" dst="${@: -1}"
cp -r "$src" "$GATEHOST_HOME/${dst#*:}"
EOF
cat > "$TMP/ssh-shim" <<'EOF'
#!/usr/bin/env bash
# ssh <host> <command>: the command runs in the fake host's home, through a login shell that is not bash's
# own quoting (sh -c), with the gate bypass exported on the host side.
[ -f "$GATEHOST_HOME/unreachable" ] && { echo "ssh: connect to host $1 port 22: No route to host" >&2; exit 255; }
cd "$GATEHOST_HOME" && HOME="$GATEHOST_HOME" RUN_AT_REF_NO_NIX=1 CONCRETE_SKIP_GATES=1 sh -c "$2"
EOF
chmod +x "$TMP/scp-shim" "$TMP/ssh-shim"
INJ="$(printf 'one\nCONCRETE_Y; touch %s/PWNED; #=two $(touch %s/PWNED2) %s %s' "$TMP" "$TMP" "'q'" '"d"')"
e2e(){ # -> runs gates_remote.sh in $RI for base..head; output in $out, status in $rc
  out="$(cd "$RI" && GATEHOST_HOME="$HH" LAND_GATES_SSH="$TMP/ssh-shim" LAND_GATES_SCP="$TMP/scp-shim" \
    CONCRETE_TEST_OUT="$TMP" CONCRETE_TEST_LIB="$ROOT_DIR/scripts/checks/lib/prepush_record.sh" CONCRETE_INJ="$INJ" \
    bash "$ROOT_DIR/scripts/land/gates_remote.sh" --host fakehost --base "$RIB" --head "$RIH" 2>&1)"; rc=$?
}
e2e
RIATT="$(cd "$RI" && git rev-parse --git-path concrete-prepush-remote-record)"; case "$RIATT" in /*) ;; *) RIATT="$RI/$RIATT" ;; esac
[ "$rc" = 0 ] && has "$out" "sha=$RIH clone=" && has "$out" "stub gates passed" && grep -qx "head=$RIH" "$RIATT" 2>/dev/null \
  && ok "the real transport runs the head on the host and the pass is attested" || { no "e2e transport: exit $rc"; echo "$out" | sed 's/^/      /'; }
[ ! -e "$TMP/PWNED" ] && [ ! -e "$TMP/PWNED2" ] && ! grep -qz '^CONCRETE_Y' "$TMP/remote.env" && grep -qz '^CONCRETE_TEST_LIB=' "$TMP/remote.env" \
  && ok "a forwarded value with newlines, \$( ) and quotes runs nothing on the host" || no "forwarded env executed on the host"
python3 -I -c 'import sys; sys.exit(0 if ("CONCRETE_INJ=" + sys.argv[2]).encode() in open(sys.argv[1], "rb").read().split(b"\0") else 1)' "$TMP/remote.env" "$INJ" && ok "the forwarded value arrives byte for byte" || no "forwarded value altered"
! grep -qz '^CONCRETE_SKIP_GATES=' "$TMP/remote.env" && ok "the host's own CONCRETE_SKIP_GATES is cleared before the hook" || no "host-side bypass reached the hook"
[ -z "$(ls -A "$HH" | grep -v '^concrete2$\|^runs$')" ] && ok "the host keeps no bundle or script after the run" || no "host leftovers: $(ls -A "$HH")"
printf 'scripts/g2.sh\n' > "$TMP/ri.changed"; printf 'bash scripts/g.sh\n' > "$TMP/ri.plan"
dec="$( cd "$RI" && export CONCRETE_TEST_OUT="$TMP" CONCRETE_TEST_LIB="$ROOT_DIR/scripts/checks/lib/prepush_record.sh" CONCRETE_INJ="$INJ" \
  && source "$ROOT_DIR/scripts/checks/lib/prepush_record.sh" && ppr_remote_components "$RI" "$RIH" "$TMP/ri.changed" "$TMP/ri.plan" "$TMP/ri.comp" \
  && ppr_remote_check "$RI" "$TMP/ri.comp" && echo REUSE || echo "refused:$PPR_REASON" )"
[ "$dec" = REUSE ] && ok "the push's hook reuses the record the host's run produced" || no "e2e reuse: $dec"
dec="$( cd "$RI" && source "$ROOT_DIR/scripts/checks/lib/prepush_record.sh" && ppr_remote_components "$RI" "$RIH" "$TMP/ri.changed" "$TMP/ri.plan" "$TMP/ri.comp" \
  && ppr_remote_check "$RI" "$TMP/ri.comp" && echo REUSE || echo "refused:$PPR_REASON" )"
has "$dec" "refused:changed:env" && ok "a push in another CONCRETE_* environment than the host's run does not reuse it" || no "e2e env mismatch: $dec"
# The login is checked on the host before anything is fetched: a root login, an unreadable one, or
# one without its gate clone is INCOMPLETE and named (2026-11-08: the root admin alias looked for
# /root/concrete2). A git shim on the "host" logs every fetch; each case must log none.
mkdir -p "$TMP/hostgit" "$TMP/hostroot" "$TMP/hostnoid"
printf '#!/bin/sh\nfor a in "$@"; do case "$a" in fetch) echo "$*" >> "%s"; break ;; esac; done\nexec "%s" "$@"\n' "$TMP/hostgit.log" "$(command -v git)" > "$TMP/hostgit/git"
printf '#!/bin/sh\ncase "$1" in -u) echo 0 ;; -un) echo root ;; *) exit 1 ;; esac\n' > "$TMP/hostroot/id"
printf '#!/bin/sh\nexit 1\n' > "$TMP/hostnoid/id"
chmod +x "$TMP/hostgit/git" "$TMP/hostroot/id" "$TMP/hostnoid/id"
: > "$TMP/hostgit.log"; PATH="$TMP/hostroot:$TMP/hostgit:$PATH" e2e
[ "$rc" = 3 ] && has "$out" "INCOMPLETE" && has "$out" "gate host login is root@" && [ ! -s "$TMP/hostgit.log" ] && [ ! -f "$RIATT" ] \
  && ok "a root login on the gate host is refused before it fetches (INCOMPLETE)" || { no "root gate login: exit $rc"; echo "$out" | sed 's/^/      /'; }
: > "$TMP/hostgit.log"; PATH="$TMP/hostnoid:$TMP/hostgit:$PATH" e2e
[ "$rc" = 3 ] && has "$out" "INCOMPLETE" && has "$out" "cannot read the gate host login" && [ ! -s "$TMP/hostgit.log" ] && [ ! -f "$RIATT" ] \
  && ok "a gate host login that id cannot read is refused, not taken as non-root (INCOMPLETE)" || { no "unreadable gate login: exit $rc"; echo "$out" | sed 's/^/      /'; }
mv "$HH/concrete2" "$HH/concrete2.away"; mkdir "$HH/concrete2"; : > "$TMP/hostgit.log"
PATH="$TMP/hostgit:$PATH" e2e
rmdir "$HH/concrete2"; mv "$HH/concrete2.away" "$HH/concrete2"
[ "$rc" = 3 ] && has "$out" "INCOMPLETE" && has "$out" "no gate clone at $HH/concrete2 for " && [ ! -s "$TMP/hostgit.log" ] && [ ! -f "$RIATT" ] \
  && ok "a gate login without its gate clone is INCOMPLETE and names the account and path" || { no "missing gate clone: exit $rc"; echo "$out" | sed 's/^/      /'; }
# A gate clone that is a linked worktree (its .git a file) is a gate clone.
mv "$HH/concrete2" "$HH/concrete2.main"; git -C "$HH/concrete2.main" worktree add -q --detach "$HH/concrete2" 2>/dev/null
e2e
git -C "$HH/concrete2.main" worktree remove --force "$HH/concrete2"; mv "$HH/concrete2.main" "$HH/concrete2"
[ "$rc" = 0 ] && has "$out" "stub gates passed" \
  && ok "a gate clone that is a linked worktree runs the gates" || { no "worktree gate clone: exit $rc"; echo "$out" | sed 's/^/      /'; }
touch "$HH/unreachable"; e2e; rm -f "$HH/unreachable"
[ "$rc" = 3 ] && has "$out" "INCOMPLETE" && [ ! -f "$RIATT" ] && ok "the real transport to an unreachable host is INCOMPLETE (exit 3), attests nothing" || { no "e2e unreachable: exit $rc"; echo "$out" | sed 's/^/      /'; }
hook="$ROOT_DIR/.githooks/pre-push"
# The root hook has no gate selection: every push that can reach main or ci/* builds the spec and
# runs the whole root set (scripts/checks/run.sh), so there is no per-push selection for a remote
# attestation to stand in for. It must therefore never consult one, and must run the full set.
! grep -q "ppr_remote_check\|ppr_remote_components" "$hook" && grep -qxF "lake build >\"\$BUILD_LOG\" 2>&1 || { echo \"pre-push: BUILD FAILED — see \$BUILD_LOG\" >&2; exit 1; }" "$hook" \
  && grep -qF "bash scripts/checks/run.sh ||" "$hook" \
  && ok "the root pre-push hook builds the spec and runs the full root set; it consults no remote attestation" || no "hook wiring"
bash "$TRAIN" check t1 --no-remote >/dev/null 2>&1 || no "re-check after the gate-host legs"
out="$(bash "$TRAIN" --dry-run ci t1 2>&1)"; rc=$?
[ "$rc" = 0 ] && has "$out" "would dispatch" && [ -z "$(git ls-remote origin refs/heads/ci/t1)" ] \
  && ok "dry-run ci prints the push and dispatch and does neither" || no "dry-run ci: exit $rc"
printf '[{"databaseId": 4242, "headSha": "%s", "createdAt": "9999-01-01T00:00:00Z"}]' "$HEAD1" > "$FAKE_GH_DIR/list.json"
out="$(bash "$TRAIN" ci t1 2>&1)"; rc=$?
[ "$rc" = 0 ] && [ "$(git ls-remote origin refs/heads/ci/t1 | cut -f1)" = "$HEAD1" ] && has "$(cat "$TMP/state/t1/train.env")" "run_id=4242" \
  && has "$(cat "$FAKE_GH_DIR/calls.log")" "workflow run CI -R test/repo --ref ci/t1 -f base=$BASE" \
  && ok "ci pushes ci/t1 at the head, dispatches it with the train's base, records run 4242" || { no "ci: exit $rc"; echo "$out" | sed 's/^/      /'; }

runjson(){ # sha landing-conclusion campaign-conclusion
  printf '{"status":"completed","conclusion":"%s","headSha":"%s","jobs":[{"name":"Build","conclusion":"%s","databaseId":11},{"name":"Campaign: gate mutation shard 3","conclusion":"%s","databaseId":12}]}' \
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
! has "$calls" "run cancel" && has "$out" "records run 4242" \
  && ok "land cancels no run; main's own run records the validating run" || { no "land touched main's run"; echo "$out" | sed 's/^/      /'; }

echo "=== dropped: a push's result is read from the remote, and a failed push is retried ==="
# The pre-push hook stands in for GitHub dropping the connection: drop.always fails every push
# before the remote moves; drop.before fails one push before it moves; drop.after updates the
# remote itself and then fails, as a push does whose connection drops after the update.
{ echo '#!/usr/bin/env bash'; echo "$IDENTIFY"; } > "$TMP/nohooks/pre-push"
cat >> "$TMP/nohooks/pre-push" <<'EOF'
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
touch "$FAKE_GH_DIR/drop.always"
out="$(bash "$TRAIN" land t8 2>&1)"; rc=$?
rm -f "$FAKE_GH_DIR/drop.always"
[ "$rc" != 0 ] && has "$out" "failed 3 time(s)" && [ "$(git ls-remote origin refs/heads/main | cut -f1)" = "$MAIN8" ] \
  && [ ! -d "$(git rev-parse --git-common-dir)/concrete-push-both.lock" ] \
  && ok "exhausted retries fail the landing, move nothing and release the lock" || { no "exhausted retries: exit $rc"; echo "$out" | sed 's/^/      /'; }
touch "$FAKE_GH_DIR/drop.before" "$FAKE_GH_DIR/drop.after"
out="$(bash "$TRAIN" land t8 2>&1)"; rc=$?
[ "$rc" = 0 ] && has "$out" "retrying (1 of 3)" && has "$out" "the connection dropped after the update" \
  && [ "$(git ls-remote origin refs/heads/main | cut -f1)" = "$HEAD8" ] && [ "$(git ls-remote lambdaclass refs/heads/main | cut -f1)" = "$HEAD8" ] \
  && ok "land retries a push dropped before the update, accepts one dropped after it, and both remotes reach the head" || { no "dropped land: exit $rc"; echo "$out" | sed 's/^/      /'; }
[ ! -f "$FAKE_GH_DIR/drop.before" ] && [ ! -f "$FAKE_GH_DIR/drop.after" ] && ok "both dropped-connection seams fired" || no "a dropped-connection seam never fired (vacuous leg)"
standin_hook

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

echo "=== the campaign tier train.sh assumes is the workflow's campaign tier ==="
# watch treats a skipped campaign job as fine and a skipped landing job as no verdict, so a campaign job
# missing from CAMPAIGN_RE makes every ordinary run "incomplete" and no train can land (2026-11-03:
# "Checker pin freshness (campaign finding)"); a landing job wrongly listed would let a skip land.
re_default="$(sed -n 's/^CAMPAIGN_RE="\${LAND_CAMPAIGN_JOBS:-\(.*\)}"$/\1/p' "$ROOT_DIR/scripts/land/train.sh")"
out="$(python3 -I - "$ROOT_DIR/.github/workflows/lean_action_ci.yml" "$re_default" <<'PY'
import re, sys, yaml
jobs = yaml.safe_load(open(sys.argv[1]))["jobs"]
rx = re.compile(sys.argv[2])
camp = {k for k, j in jobs.items() if "inputs.campaign" in str(j.get("if", ""))}
grew = True
while grew:  # a job that needs a campaign job runs only in the campaign tier too
    grew = False
    for k, j in jobs.items():
        needs = j.get("needs", [])
        needs = [needs] if isinstance(needs, str) else needs
        if k not in camp and camp.intersection(needs):
            camp.add(k); grew = True
# The root has no campaign tier yet (lean_action_ci.yml), so an empty campaign set is the expected one; the
# comparison below still refuses any landing job the regex matches.
for k, j in jobs.items():
    name = str(j.get("name", k))
    if (k in camp) != bool(rx.search(name)):
        print(("campaign job not in CAMPAIGN_RE: " if k in camp else "landing job matched by CAMPAIGN_RE: ") + name)
PY
)"; rc=$?
[ "$rc" = 0 ] && [ -n "$re_default" ] && [ -z "$out" ] \
  && ok "CAMPAIGN_RE names exactly the workflow's campaign jobs" \
  || { no "CAMPAIGN_RE disagrees with the workflow (rc=$rc, regex '${re_default}')"; echo "$out" | sed 's/^/      /'; }

echo "=== required checks: missing, skipped, cancelled, dropped ==="
git checkout -q train/t1 2>/dev/null
HEADR="$(git rev-parse HEAD)"
reqjson(){ # sha jobs-json
  printf '{"status":"completed","conclusion":"success","headSha":"%s","jobs":%s}' "$1" "$2" > "$FAKE_GH_DIR/run.json"
}
req_leg(){ # label jobs-json expected-text
  reqjson "$HEADR" "$2"
  local o r
  o="$(bash "$TRAIN" watch t1 --interval 1 2>&1)"; r=$?
  [ "$r" = 1 ] && has "$o" "$3" && ok "watch refuses $1" || { no "watch accepted $1 (exit $r)"; echo "$o" | sed 's/^/      /'; }
  o="$(bash "$TRAIN" --dry-run land t1 2>&1)"; r=$?
  [ "$r" != 0 ] && has "$o" "$3" && ok "land refuses $1" || { no "land accepted $1 (exit $r)"; echo "$o" | sed 's/^/      /'; }
}
req_leg "a run missing a required job" '[{"name":"Other","conclusion":"success","databaseId":21}]' "missing: Build"
req_leg "a skipped required job" '[{"name":"Build","conclusion":"skipped","databaseId":21},{"name":"Other","conclusion":"success","databaseId":22}]' "skipped: Build"
req_leg "a cancelled required job" '[{"name":"Build","conclusion":"cancelled","databaseId":21},{"name":"Other","conclusion":"success","databaseId":22}]' "cancelled: Build"
req_leg "a run whose only other job is a campaign-tier one" '[{"name":"Other","conclusion":"success","databaseId":22},{"name":"Campaign: gate mutation shard 4","conclusion":"success","databaseId":23}]' "missing: Build"
# The rule itself (scripts/land/required_checks.py) on file contents: dropping, transitions, fusing.
RC="$ROOT_DIR/scripts/land/required_checks.py"
RD="$TMP/rc"; mkdir -p "$RD"
rc_run(){ # base cand trans jobs(lines "landing\tconclusion\tid\tname") -> findings, space-joined
  printf '%b' "$1" > "$RD/b"; printf '%b' "$2" > "$RD/c"; printf '%b' "$3" > "$RD/t"; printf '%b' "$4" > "$RD/j"
  python3 -I "$RC" "$RD/b" "$RD/c" "$RD/t" "$RD/j" | tr '\n' '|'
}
OK_BUILD='landing\tsuccess\t1\tBuild\n'; OK_BOTH='landing\tsuccess\t1\tBuild\nlanding\tsuccess\t2\tOther\n'
OK_NEW='landing\tsuccess\t1\tOther\nlanding\tsuccess\t2\tNew\n'
got="$(rc_run 'Build\n' 'Other\n' '' "$OK_NEW")"
has "$got" "missing: Build" && ok "a candidate that drops a base requirement without a transition row is still held to it" || no "silent drop: '$got'"
got="$(rc_run 'Build\n' 'Other\nNew\n' 'Build\tNew\tBuild moved\n' "$OK_NEW")"
[ "$got" = "replaced: Build by New|" ] && ok "a transition row replacing a base job by a required, green job passes and says so" || no "replacement: '$got'"
got="$(rc_run 'Build\n' 'Other\nNew\n' 'Build\tNew\tBuild moved\n' 'landing\tsuccess\t1\tOther\nlanding\tfailure\t2\tNew\n')"
has "$got" "failure: New" && ok "a replacement job that failed at this SHA refuses" || no "red replacement: '$got'"
got="$(rc_run 'Build\n' 'Other\n' 'Build\tNew\tBuild moved\n' "$OK_NEW")"
has "$got" "invalid: replacement New for Build is not in the candidate" && has "$got" "missing: Build" \
  && ok "a replacement the candidate does not itself require is refused, and the old job stays required" || no "unlisted replacement: '$got'"
got="$(rc_run 'Build\nLegacy\n' 'Build\n' 'Legacy\t-\tlegacy pipeline frozen (P5)\n' "$OK_BUILD")"
has "$got" "retired: Legacy (legacy pipeline frozen (P5)); not run, not passed" && ! has "$got" "missing" \
  && ok "a retired base job is reported as retired, not run, never as passed" || no "retirement: '$got'"
got="$(rc_run 'Build\nLegacy\n' 'Build\n' 'Legacy\t-\t\n' "$OK_BUILD")"
has "$got" "invalid: transition row 1 needs" && ok "a retirement without a reason is refused" || no "reasonless retirement: '$got'"
got="$(rc_run 'Build\nLegacy\n' 'Build\n' 'Legacy\t,\tmeant a list\n' "$OK_BUILD")"
has "$got" "invalid: transition row 1 has an empty replacement name" && has "$got" "missing: Legacy" && ! has "$got" "retired" \
  && ok "a malformed replacement list is refused, never read as a retirement" || no "malformed list: '$got'"
got="$(rc_run 'Build\n' 'Build\n' 'Build\t-\tno\n' "$OK_BUILD")"
has "$got" "invalid: transition row for Build, which the candidate still requires" && ok "a transition row for a job still required is refused" || no "contradictory row: '$got'"
got="$(rc_run '' '' '' "$OK_BUILD")"
has "$got" "invalid: required-checks.txt (neither" && ok "no required-checks file anywhere is refused, not read as nothing required" || no "no file: '$got'"
got="$(rc_run 'Build' 'Other' '' "$OK_BOTH")"
[ "$got" = "" ] && ok "files without a final newline do not fuse names (Build and Other both required, both green)" || no "newline-free: '$got'"
got="$(rc_run 'Build' 'Other' '' 'landing\tsuccess\t1\tBuildOther\n')"
has "$got" "missing: Build" && has "$got" "missing: Other" && ok "a job named BuildOther satisfies neither Build nor Other" || no "fused names: '$got'"
# The same rule through train.sh: a transition row on the candidate replaces the base's Build.
git checkout -q -b req-trans "$HEADR"
printf 'Other\nNew\n' > required-checks.txt
printf 'old_job\treplacements\treason\nBuild\tNew\tthe new root builds differently\n' > required-checks-transition.tsv
git add required-checks.txt required-checks-transition.tsv; git commit -qm "transition Build -> New"
HT="$(git rev-parse HEAD)"
o="$(cd "$W" && REQUIRED_FILE=x bash -c 'set -uo pipefail; SELF_DIR="$1"; REQUIRED_FILE=required-checks.txt; TRANSITION_FILE=required-checks-transition.tsv
  eval "$(sed -n "/^required_report() {/,/^}/p; /^unmet_required() {/,/^}/p" "$2")"; unmet_required "$3" "$4" "$5"' _ "$ROOT_DIR/scripts/land" "$TRAIN" "$BASE" "$HT" "$(printf 'landing\tsuccess\t1\tOther\nlanding\tsuccess\t2\tNew')")"
[ -z "$o" ] && ok "train.sh reads the candidate's transition file at the head commit" || no "train.sh transition: '$o'"
git checkout -q train/t1

echo "=== record: main's run names the run that qualified its exact SHA, and nothing else (R-0570) ==="
# train.sh record HEAD --base BASE: the toy main moved BASE -> HEADR; its required-checks.txt asks for
# Build. Run 501 is the train's dispatch at HEADR. Each leg changes one thing and must be refused.
RW="$FAKE_GH_DIR"
rrun(){ # id head path event status [title] -> one runs-API entry; the title defaults to a train's
  printf '{"id": %s, "head_sha": "%s", "path": "%s", "event": "%s", "status": "%s", "display_title": "%s"}' \
    "$1" "$2" "$3" "$4" "$5" "${6:-ci/t1 at base $BASE}"
}
WF=.github/workflows/lean_action_ci.yml
rruns(){ printf '{"workflow_runs": [%s]}' "$(IFS=,; echo "$*")" > "$RW/runs.json"; }
rjobs(){ # id sha jobs-json
  printf '{"status":"completed","conclusion":"success","headSha":"%s","jobs":%s}' "$2" "$3" > "$RW/run.$1.json"
}
GREEN='[{"name":"Build","conclusion":"success","databaseId":31},{"name":"Other","conclusion":"success","databaseId":32},{"name":"Campaign: gate mutation shard 1","conclusion":"skipped","databaseId":33}]'
rec(){ bash "$TRAIN" record "$@" 2>&1; }
rec_refuses(){ # label expected-text args...
  local label="$1" want="$2" o r; shift 2
  o="$(rec "$@")"; r=$?
  [ "$r" != 0 ] && has "$o" "$want" && ! has "$o" "qualifying_run=" && ok "record refuses $label" \
    || { no "record accepted $label (exit $r)"; echo "$o" | sed 's/^/      /'; }
}
reset_record(){
  echo '{"status": "ahead"}' > "$RW/compare.json"
  rruns "$(rrun 501 "$HEADR" "$WF" workflow_dispatch completed)"
  rjobs 501 "$HEADR" "$GREEN"
}
reset_record
o="$(rec "$HEADR" --base "$BASE" --exclude-run 900)"; r=$?
[ "$r" = 0 ] && has "$o" "qualifying_run=501" && ok "record: the newest green run at exactly the head qualifies it (a skipped campaign job is fine)" \
  || { no "record refused a qualifying run (exit $r)"; echo "$o" | sed 's/^/      /'; }
rruns ''
rec_refuses "with no run at the head" "no completed CI dispatch" "$HEADR" --base "$BASE"
rruns "$(rrun 501 "$BASE" "$WF" workflow_dispatch completed)"
rec_refuses "a run at another SHA" "no completed CI dispatch" "$HEADR" --base "$BASE"
rruns "$(rrun 501 "$HEADR" "$WF" workflow_dispatch completed)"; rjobs 501 "$BASE" "$GREEN"
rec_refuses "a run the API lists at the head but whose own record names another SHA" "ran ${BASE:0:10}" "$HEADR" --base "$BASE"
reset_record; rruns "$(rrun 501 "$HEADR" .github/workflows/other.yml workflow_dispatch completed)"
rec_refuses "a run of another workflow" "no completed CI dispatch" "$HEADR" --base "$BASE"
rruns "$(rrun 501 "$HEADR" "$WF" pull_request completed)"
rec_refuses "a pull_request run" "no completed CI dispatch" "$HEADR" --base "$BASE"
rruns "$(rrun 501 "$HEADR" "$WF" push completed)"
rec_refuses "a push run (its checks' base is not the declared one)" "no completed CI dispatch" "$HEADR" --base "$BASE"
rruns "$(rrun 501 "$HEADR" "$WF" workflow_dispatch completed "ci/t1 at base $HEADR")"
rec_refuses "a dispatch whose checks ran at another base" "no completed CI dispatch" "$HEADR" --base "$BASE"
rruns "$(rrun 501 "$HEADR" "$WF" workflow_dispatch completed "CI")"
rec_refuses "a dispatch that names no base (a campaign or hand dispatch)" "no completed CI dispatch" "$HEADR" --base "$BASE"
rruns "$(rrun 501 "$HEADR" "$WF" workflow_dispatch in_progress)"
rec_refuses "an unfinished run" "no completed CI dispatch" "$HEADR" --base "$BASE"
rruns "$(rrun 501 "$HEADR" "$WF" workflow_dispatch completed)"
rec_refuses "the asking run itself" "no completed CI dispatch" "$HEADR" --base "$BASE" --exclude-run 501
reset_record
rjobs 501 "$HEADR" '[{"name":"Build","conclusion":"skipped","databaseId":31},{"name":"Other","conclusion":"success","databaseId":32}]'
rec_refuses "a skipped required job" "skipped: Build" "$HEADR" --base "$BASE"
rjobs 501 "$HEADR" '[{"name":"Build","conclusion":"cancelled","databaseId":31},{"name":"Other","conclusion":"success","databaseId":32}]'
rec_refuses "a cancelled required job" "cancelled: Build" "$HEADR" --base "$BASE"
rjobs 501 "$HEADR" '[{"name":"Build","conclusion":"success","databaseId":31},{"name":"Other","conclusion":"failure","databaseId":32}]'
rec_refuses "a failed landing-tier job" "failure: Other" "$HEADR" --base "$BASE"
rjobs 501 "$HEADR" '[{"name":"Other","conclusion":"success","databaseId":32}]'
rec_refuses "a run without a required job" "missing: Build" "$HEADR" --base "$BASE"
rjobs 501 "$HEADR" '[{"name":"Campaign: gate mutation shard 1","conclusion":"success","databaseId":33}]'
rec_refuses "a run with no landing-tier job" "no landing-tier jobs" "$HEADR" --base "$BASE"
reset_record
rruns "$(rrun 502 "$HEADR" "$WF" workflow_dispatch completed)" "$(rrun 501 "$HEADR" "$WF" workflow_dispatch completed)"
rjobs 502 "$HEADR" '[{"name":"Build","conclusion":"failure","databaseId":41}]'
rec_refuses "an older green run behind a newer red one at the same SHA" "run 502, the newest" "$HEADR" --base "$BASE"
reset_record; echo '{"status": "diverged"}' > "$RW/compare.json"
rec_refuses "a head not strictly ahead of the base" "compare: diverged" "$HEADR" --base "$BASE"
rm -f "$RW/compare.json"
rec_refuses "when the compare API cannot be read" "compare: -" "$HEADR" --base "$BASE"
reset_record
rec_refuses "a base this clone does not have" "not in this clone" "$HEADR" --base 0123456789abcdef0123456789abcdef01234567
rec_refuses "an all-zero base (a new branch, not a landing)" "--base must be" "$HEADR" --base 0000000000000000000000000000000000000000
rec_refuses "an abbreviated head" "full 40-hex SHA" "${HEADR:0:10}" --base "$BASE"
rm -f "$RW/runs.json"
rec_refuses "when the runs API cannot be read" "no completed CI dispatch" "$HEADR" --base "$BASE"
# A base requirement the head drops without a transition row is still owed (land's own rule).
git checkout -q -b rec-drop "$HEADR"; printf 'Other\n' > required-checks.txt; git commit -qam "drop Build"
HD="$(git rev-parse HEAD)"; git checkout -q train/t1
reset_record; rruns "$(rrun 503 "$HD" "$WF" workflow_dispatch completed)"
rjobs 503 "$HD" '[{"name":"Other","conclusion":"success","databaseId":51}]'
rec_refuses "a head that drops a base requirement silently" "missing: Build" "$HD" --base "$BASE"
reset_record

echo "=== the workflow: Landing record gates every landing-tier job; a dispatch's base binds its checks ==="
cat > "$TMP/record_gate.py" <<'PY2'
import re, sys, yaml
doc = yaml.safe_load(open(sys.argv[1]))
jobs = doc["jobs"]
rx = re.compile(sys.argv[2])
# The record trusts a dispatch's title for its base: the title must come from the base input, and
# every base-sensitive check must compare with that input as a push compares with event.before.
if "format('{0} at base {1}', github.ref_name, inputs.base)" not in str(doc.get("run-name", "")):
    print("run-name does not record a dispatch's base as '<ref> at base <sha>'")
bases = 0
for k, j in jobs.items():
    for st in j.get("steps", []):
        for var, val in (st.get("env") or {}).items():
            if var in ("LL1_BASE", "TOTALITY_BASE", "CONFORMANCE_BASE", "SPEC_BASE"):
                bases += 1
                if "github.event.before" not in str(val) or "inputs.base" not in str(val):
                    print(f"{var} in {k} does not take a push's event.before and a dispatch's inputs.base")
GATE = "needs.landing-record.outputs.recorded != 'true'"
rec = jobs.get("landing-record")
if not rec or rec.get("name") != "Landing record":
    print("no job landing-record named 'Landing record'")
else:
    if rec.get("needs") or rec.get("if"):
        print("landing-record must run on every event, first")
    if "steps.record.outputs.recorded" not in str(rec.get("outputs", {}).get("recorded", "")):
        print("landing-record does not export the record step's verdict")
def needs(j):
    n = j.get("needs", [])
    return [n] if isinstance(n, str) else list(n)
gated = {}
def is_gated(k, seen=()):
    if k in gated: return gated[k]
    j = jobs[k]; n = needs(j); cond = str(j.get("if", ""))
    if "landing-record" in n:
        r = GATE in cond and "!cancelled()" in cond
    elif n:
        # a job that needs others runs only when they ran; if: always()/!cancelled() would undo that
        r = all(m in jobs and m not in seen and is_gated(m, seen + (k,)) for m in n) \
            and not re.search(r"always\(\)|cancelled\(\)|failure\(\)", cond)
    else:
        r = False
    gated[k] = r; return r
for k, j in jobs.items():
    if k == "landing-record" or rx.search(str(j.get("name", k))):
        continue
    if not is_gated(k):
        print("landing-tier job not gated on the landing record: " + str(j.get("name", k)))
PY2
out="$(python3 -I "$TMP/record_gate.py" "$ROOT_DIR/.github/workflows/lean_action_ci.yml" "$re_default")"; rc=$?
[ "$rc" = 0 ] && [ -z "$out" ] && ok "every landing-tier job is skipped exactly when main's run holds a record" \
  || { no "the workflow's record gate is incomplete (rc=$rc)"; echo "$out" | sed 's/^/      /'; }
# Controls: an ungated root job, a dependent that re-runs on always(), and a gate without
# !cancelled() (a failed record job would then skip every job) are each found.
python3 -I - "$ROOT_DIR/.github/workflows/lean_action_ci.yml" "$TMP/rg" <<'PY3'
import sys, yaml, copy, os
w = yaml.safe_load(open(sys.argv[1])); os.makedirs(sys.argv[2], exist_ok=True)
a = copy.deepcopy(w); a["jobs"]["spec"].pop("needs"); a["jobs"]["spec"].pop("if")
b = copy.deepcopy(w); b["jobs"]["oracle-hosted"]["if"] = "always()"
c = copy.deepcopy(w); c["jobs"]["root-checks"]["if"] = "needs.landing-record.outputs.recorded != 'true'"
d = copy.deepcopy(w)
d["jobs"]["root-checks"]["steps"].append({"run": "true", "env": {"TOTALITY_BASE": "${{ github.event.before }}"}})
for j in d["jobs"].values():
    for st in j.get("steps", []):
        if "TOTALITY_BASE" in (st.get("env") or {}):
            st["env"]["TOTALITY_BASE"] = "${{ github.event_name == 'push' && github.event.before || '' }}"
for n, x in (("ungated", a), ("always", b), ("nocancel", c), ("unbased", d)):
    yaml.safe_dump(x, open(os.path.join(sys.argv[2], n + ".yml"), "w"))
PY3
o="$(python3 -I "$TMP/record_gate.py" "$TMP/rg/unbased.yml" "$re_default")"
has "$o" "TOTALITY_BASE in" && ok "CONTROL: the record-gate check finds a base-sensitive check that ignores the dispatch's base" \
  || no "CONTROL: the record-gate check missed an unbased check: '$o'"
for v in ungated always nocancel; do
  o="$(python3 -I "$TMP/record_gate.py" "$TMP/rg/$v.yml" "$re_default")"
  has "$o" "not gated" && ok "CONTROL: the record-gate check finds the $v job" || no "CONTROL: the record-gate check missed the $v job: '$o'"
done

echo "=== required-checks.txt names exactly the workflow's landing-tier jobs ==="
# A matrix job is one job per leg, each under the name GitHub shows: its name template with that
# leg's values (`include` rows only; any other matrix form is refused, since its legs are not listed).
cat > "$TMP/required_names.py" <<'PY'
import re, sys, yaml
jobs = yaml.safe_load(open(sys.argv[1]))["jobs"]
rx = re.compile(sys.argv[2])
def legs(k, j):
    name = str(j.get("name", k)); m = (j.get("strategy") or {}).get("matrix")
    if m is None:
        return [name]
    if set(m) != {"include"} or not isinstance(m["include"], list) or not m["include"]:
        print(f"matrix of {k} is not a non-empty include list: its legs cannot be named"); return []
    out = []
    for row in m["include"]:
        leg = re.sub(r"\$\{\{\s*matrix\.(\w+)\s*\}\}", lambda g: str(row.get(g.group(1), "\0")), name)
        if "\0" in leg or "${{" in leg:
            print(f"matrix leg {row} of {k} leaves its name unresolved: {name}"); continue
        out.append(leg)
    if len(set(out)) != len(m["include"]):
        print(f"matrix legs of {k} do not have distinct names: {out}")
    return out
landing = {n for k, j in jobs.items() for n in legs(k, j) if not rx.search(n)}
req = {l.split("#")[0].strip() for l in open(sys.argv[3])} - {""}
for n in sorted(req - landing): print("required but not a landing-tier job: " + n)
for n in sorted(landing - req): print("landing-tier job not required: " + n)
PY
out="$(python3 -I "$TMP/required_names.py" "$ROOT_DIR/.github/workflows/lean_action_ci.yml" "$re_default" "$ROOT_DIR/required-checks.txt")"; rc=$?
[ "$rc" = 0 ] && [ -z "$out" ] && ok "required-checks.txt = the workflow's landing tier, one name per matrix leg" || { no "required-checks.txt disagrees with the workflow (rc=$rc)"; echo "$out" | sed 's/^/      /'; }
# Controls: a matrix leg missing from required-checks.txt, a leg whose name the template does not
# resolve, and a cross-product matrix are each reported.
python3 -I - "$ROOT_DIR/.github/workflows/lean_action_ci.yml" "$TMP/rn" <<'PY3'
import sys, yaml, copy, os
w = yaml.safe_load(open(sys.argv[1])); os.makedirs(sys.argv[2], exist_ok=True)
a = copy.deepcopy(w); a["jobs"]["backends"]["strategy"]["matrix"]["include"].append({"arch": "riscv64", "runner": "ubuntu-latest"})
b = copy.deepcopy(w); b["jobs"]["backends"]["name"] = "Slice 0 backends (${{ matrix.os }})"
c = copy.deepcopy(w); c["jobs"]["backends"]["strategy"]["matrix"] = {"arch": ["x86-64", "aarch64-macos"]}
for n, x in (("extraleg", a), ("unresolved", b), ("product", c)):
    yaml.safe_dump(x, open(os.path.join(sys.argv[2], n + ".yml"), "w"))
PY3
for v in extraleg:"landing-tier job not required: Slice 0 backends (riscv64)" unresolved:"leaves its name unresolved" product:"is not a non-empty include list"; do
  o="$(python3 -I "$TMP/required_names.py" "$TMP/rn/${v%%:*}.yml" "$re_default" "$ROOT_DIR/required-checks.txt")"
  has "$o" "${v#*:}" && ok "CONTROL: the required-checks check reports the ${v%%:*} matrix" || no "CONTROL: the required-checks check missed the ${v%%:*} matrix: '$o'"
done

echo "=== the move gate: legacy/ lands only with an accepted rehearsal of that exact candidate ==="
cd "$W" || exit 2
git checkout -q main && git fetch -q origin && git reset -q --hard origin/main
MB="$(git rev-parse HEAD)"
git checkout -q -b bmove "$MB"; mkdir -p legacy; printf 'old tree\n' > legacy/old.txt; commit_all bmove
out="$(bash "$TRAIN" build tm bmove 2>&1)" || { no "move train build"; echo "$out" | sed 's/^/      /'; }
git checkout -q train/tm
HM="$(git rev-parse HEAD)"; TM="$(git rev-parse 'HEAD^{tree}')"
bash "$TRAIN" check tm --no-remote >/dev/null 2>&1 || no "move train check"
printf '[{"databaseId": 5151, "headSha": "%s", "createdAt": "9999-01-01T00:00:00Z"}]' "$HM" > "$FAKE_GH_DIR/list.json"
bash "$TRAIN" ci tm >/dev/null 2>&1 || no "move train ci"
runjson "$HM" success success
REH="$TMP/state/rehearsals.tsv"
move_leg(){ # label expected-text: both land and dry-run land refuse, main stays put, the dry run exits 5
  local o r
  o="$(bash "$TRAIN" land tm 2>&1)"; r=$?
  [ "$r" = 1 ] && has "$o" "is a legacy/ move candidate" && has "$o" "$2" && [ "$(git ls-remote origin refs/heads/main | cut -f1)" = "$MB" ] \
    && ok "land refuses the move with $1" || { no "land with $1 (exit $r)"; echo "$o" | sed 's/^/      /'; }
  o="$(bash "$TRAIN" --dry-run land tm 2>&1)"; r=$?
  [ "$r" = 5 ] && has "$o" "every other land check passed; refused only by the move gate" && has "$o" "$2" \
    && ok "dry-run land refuses the move with $1 (exit 5, every other check passed)" || { no "dry-run land with $1 (exit $r)"; echo "$o" | sed 's/^/      /'; }
}
rm -f "$REH"
move_leg "no rehearsal record" "no rehearsal record"
printf '# header\n2026-11-05T00:00:00Z\taccepted\t%s\t%s\t%s\twip/other\tother\n' "$(git rev-parse b1)" "$(git rev-parse 'b1^{tree}')" "$MB" > "$REH"
move_leg "an accepted rehearsal of another candidate (another tree)" "the record holds other candidates only"
printf '2026-11-05T00:00:01Z\taccepted\t%s\t%s\t%s\tbmove\tbase-moved\n' "$HM" "$TM" "$BASE" >> "$REH"
move_leg "an accepted rehearsal of the same tree on another base" "the record holds other candidates only"
printf '2026-11-05T00:00:02Z\tfailed\t%s\t%s\t%s\tbmove\taccept-x\n' "$HM" "$TM" "$MB" >> "$REH"
move_leg "a failed rehearsal of this candidate" "was failed, not accepted"
printf '2026-11-05T00:00:03Z\tincomplete\t%s\t%s\t%s\tbmove\taccept-y\n' "$HM" "$TM" "$MB" >> "$REH"
move_leg "an incomplete rehearsal of this candidate" "was incomplete, not accepted"
# A corrupt row (here, the shape a newline smuggled through a field would leave) refuses outright.
cp "$REH" "$TMP/reh.good"
printf 'accepted\t%s\t%s\n' "$TM" "$MB" >> "$REH"
out="$(bash "$TRAIN" --dry-run land tm 2>&1)"; rc=$?
[ "$rc" = 1 ] && has "$out" "is not a rehearsal row" && ok "a malformed rehearsal row refuses the land (not skipped, not exit 5)" || { no "malformed row: dry-run land exit $rc"; echo "$out" | sed 's/^/      /'; }
cp "$TMP/reh.good" "$REH"
# The acceptance run builds its own train: build the same member again under another name, check the
# tree is the same, and let that train's record authorize this one.
bash "$TRAIN" build tmb bmove >/dev/null 2>&1 || no "second move train build"
HMB="$(git rev-parse train/tmb)"
[ "$HMB" != "$HM" ] && [ "$(git rev-parse 'train/tmb^{tree}')" = "$TM" ] \
  && ok "two trains of the same base and member: different merge commits, the same tree" || no "second train: head $HMB tree $(git rev-parse 'train/tmb^{tree}') vs $HM $TM"
git checkout -q train/tm
printf '2026-11-05T00:00:04Z\taccepted\t%s\t%s\t%s\tbmove\ttmb\n' "$HMB" "$TM" "$MB" >> "$REH"
out="$(bash "$TRAIN" --dry-run land tm 2>&1)"; rc=$?
[ "$rc" = 0 ] && has "$out" "acceptance accepted tree ${TM:0:10}" && [ "$(git ls-remote origin refs/heads/main | cut -f1)" = "$MB" ] \
  && ok "an accepted rehearsal by another train of the same tree and base satisfies the gate" || { no "accepted rehearsal: dry-run land exit $rc"; echo "$out" | sed 's/^/      /'; }
out="$(bash "$TRAIN" land tm 2>&1)"; rc=$?
[ "$rc" = 0 ] && [ "$(git ls-remote origin refs/heads/main | cut -f1)" = "$HM" ] \
  && ok "the rehearsed move lands" || { no "rehearsed move land: exit $rc"; echo "$out" | sed 's/^/      /'; }
# A move staged over several candidates: legacy/ is already on main, and the next candidate moves more
# into it. It adds no marker, and is still a move candidate needing its own rehearsal.
git checkout -q -b bmove3 "$HM"; printf 'more of the old tree\n' > legacy/more.txt; commit_all bmove3
bash "$TRAIN" build tm3 bmove3 >/dev/null 2>&1 || no "staged move train build"
git checkout -q train/tm3; HM3="$(git rev-parse HEAD)"
bash "$TRAIN" check tm3 --no-remote >/dev/null 2>&1 || no "staged move train check"
printf '[{"databaseId": 5353, "headSha": "%s", "createdAt": "9999-01-01T00:00:00Z"}]' "$HM3" > "$FAKE_GH_DIR/list.json"
bash "$TRAIN" ci tm3 >/dev/null 2>&1 || no "staged move train ci"
runjson "$HM3" success success
out="$(bash "$TRAIN" --dry-run land tm3 2>&1)"; rc=$?
[ "$rc" = 5 ] && has "$out" "the record holds other candidates only" \
  && ok "a staged move (legacy/ already on the base) still needs a rehearsal of its own" || { no "staged move: dry-run land exit $rc"; echo "$out" | sed 's/^/      /'; }
MB="$HM"
# acceptance.sh's dry-run leg passes a refusal only when it is exit 5 with the marker as a whole line.
(
  . "$ROOT_DIR/scripts/land/lib.sh"
  dryland_only_move_gate 5 "x"$'\n'"$MOVE_GATE_MARKER"$'\n'"train: land: ..." && echo pass1
  dryland_only_move_gate 5 "train: land: refused" || echo pass2
  dryland_only_move_gate 1 "$MOVE_GATE_MARKER" || echo pass3
  dryland_only_move_gate 4 "$MOVE_GATE_MARKER" || echo pass4
  dryland_only_move_gate 5 "note: $MOVE_GATE_MARKER" || echo pass5
  dryland_only_move_gate 5 "${MOVE_GATE_MARKER%.}" || echo pass6
) > "$TMP/dl.out" 2>&1
[ "$(tr '\n' ' ' < "$TMP/dl.out")" = "pass1 pass2 pass3 pass4 pass5 pass6 " ] \
  && ok "acceptance's dry-run leg: exit 5 + whole marker line passes; no marker, another exit, a prefixed or altered marker fail" || { no "dryland classifier: $(cat "$TMP/dl.out")"; }
# acceptance.sh refuses a --name or --ref that could forge a record row, before writing anything.
for bad in "--name x"$'\n'"y" "--ref wip/a"$'\t'"b"; do
  o="$(cd "$W" && ACCEPT_REHEARSALS="$TMP/reh.forged" ACCEPT_RECORD="$TMP/acc.forged" bash "$ROOT_DIR/scripts/land/acceptance.sh" --ref wip/x ${bad%% *} "${bad#* }" 2>&1)"; r=$?
  [ "$r" = 2 ] && has "$o" "${bad%% *} must" && [ ! -e "$TMP/reh.forged" ] && [ ! -e "$TMP/acc.forged" ] \
    && ok "acceptance refuses ${bad%% *} with a record delimiter and writes no row" || { no "acceptance ${bad%% *} delimiter: exit $r"; echo "$o" | sed 's/^/      /'; }
done
# acceptance.sh's check leg keeps the review leg (it never passes --no-review), and its build leg and
# broken-build control run the one build command, the root's.
ACC="$ROOT_DIR/scripts/land/acceptance.sh"
grep -qxF 'do_check() { TRAIN check "$NAME" --no-remote; }' "$ACC" && ! grep -qE '^[^#]*--no-review' "$ACC" \
  && grep -qxF 'BUILD_CMD="${ACCEPT_BUILD_CMD:-nix develop --command lake build}"' "$ACC" \
  && [ "$(grep -cF 'bash -c "$BUILD_CMD"' "$ACC")" = 2 ] && ! grep -qF 'make build' "$ACC" \
  && ok "acceptance: check keeps the review leg; both build legs run nix develop --command lake build" || no "acceptance check or build wiring"
# A run that fails before it has a train (here, a ref that does not exist) records its failure in
# acceptance.tsv and writes no rehearsal row, so it cannot block the next rehearsal or land.
o="$(cd "$W" && for v in $(compgen -e); do case "$v" in LAND_*|CONCRETE_SKIP_*) unset "$v" ;; esac; done
     ACCEPT_REHEARSALS="$TMP/reh.early" ACCEPT_RECORD="$TMP/acc.early" bash "$ROOT_DIR/scripts/land/acceptance.sh" --ref wip/no-such-ref 2>&1)"; r=$?
[ "$r" != 0 ] && [ ! -e "$TMP/reh.early" ] && has "$(cat "$TMP/acc.early" 2>/dev/null)" $'verdict\tfailed' \
  && has "$o" "binds no candidate" && ok "an acceptance run that fails before building a train writes no rehearsal row" || { no "early acceptance failure: exit $r"; echo "$o" | sed 's/^/      /'; }
# The gate keys on what the candidate touches, not on a name: a non-move train needs no record (t1
# landed above with none), and the new root's inventory alone also makes a move.
rm -f "$REH"
git checkout -q -b bmove2 "$MB"; mkdir -p scripts/checks; printf 'README.md\n' > scripts/checks/root_inventory.txt; commit_all bmove2
bash "$TRAIN" build tm2 bmove2 >/dev/null 2>&1 || no "inventory train build"
git checkout -q train/tm2; HM2="$(git rev-parse HEAD)"
bash "$TRAIN" check tm2 --no-remote >/dev/null 2>&1 || no "inventory train check"
printf '[{"databaseId": 5252, "headSha": "%s", "createdAt": "9999-01-01T00:00:00Z"}]' "$HM2" > "$FAKE_GH_DIR/list.json"
bash "$TRAIN" ci tm2 >/dev/null 2>&1 || no "inventory train ci"
runjson "$HM2" success success
out="$(bash "$TRAIN" --dry-run land tm2 2>&1)"; rc=$?
[ "$rc" = 5 ] && has "$out" "no rehearsal record" && ok "a candidate adding scripts/checks/root_inventory.txt is a move too" || { no "inventory move: dry-run land exit $rc"; echo "$out" | sed 's/^/      /'; }
# Only the inventory's TOP-LEVEL entry set counts. main gets an inventory first (a direct push, as
# the moved-main leg above does); then a candidate that edits only nested entries, comments and the
# order needs no rehearsal, while one that adds a top-level entry, and one that changes legacy/, does.
( cd "$TMP/other" && git fetch -q origin && git reset -q --hard origin/main && mkdir -p scripts/checks \
  && printf '# day one\nREADME.md\nlegacy\ndocs\ndocs/LANDING.md\n' > scripts/checks/root_inventory.txt \
  && git add -A && git commit -qm inventory && CONCRETE_SKIP_GATES=1 git push -q origin main )
git checkout -q main && git fetch -q origin && git reset -q --hard origin/main
MI="$(git rev-parse HEAD)"
inv_leg(){ # train branch run-id -> builds, checks and runs CI for the train; dry-run land in $out/$rc
  bash "$TRAIN" build "$1" "$2" >/dev/null 2>&1 || no "$1 build"
  git checkout -q "train/$1"; local h; h="$(git rev-parse HEAD)"
  bash "$TRAIN" check "$1" --no-remote >/dev/null 2>&1 || no "$1 check"
  printf '[{"databaseId": %s, "headSha": "%s", "createdAt": "9999-01-01T00:00:00Z"}]' "$3" "$h" > "$FAKE_GH_DIR/list.json"
  bash "$TRAIN" ci "$1" >/dev/null 2>&1 || no "$1 ci"
  runjson "$h" success success
  out="$(bash "$TRAIN" --dry-run land "$1" 2>&1)"; rc=$?
}
git checkout -q -b binv-nested "$MI"
printf '# the day-one root, reordered\ndocs/proposals\ndocs/LANDING.md\ndocs\nlegacy\nREADME.md\n' > scripts/checks/root_inventory.txt
commit_all binv-nested
inv_leg tin1 binv-nested 5454
[ "$rc" = 0 ] && ! has "$out" "move candidate" \
  && ok "an inventory change of nested entries, comments and order only (docs/proposals) needs no rehearsal" || { no "nested inventory change: dry-run land exit $rc"; echo "$out" | sed 's/^/      /'; }
git checkout -q -b binv-root "$MI"
printf '# day one\nREADME.md\nNEWS.md\nlegacy\ndocs\ndocs/LANDING.md\n' > scripts/checks/root_inventory.txt
commit_all binv-root
inv_leg tin2 binv-root 5455
[ "$rc" = 5 ] && has "$out" "no rehearsal record" \
  && ok "an inventory change that adds a top-level entry still needs a rehearsal" || { no "top-level inventory change: dry-run land exit $rc"; echo "$out" | sed 's/^/      /'; }
git checkout -q -b binv-legacy "$MI"; printf 'edited old tree\n' > legacy/old.txt; commit_all binv-legacy
inv_leg tin3 binv-legacy 5456
[ "$rc" = 5 ] && has "$out" "no rehearsal record" \
  && ok "a change under legacy/ (inventory untouched) still needs a rehearsal" || { no "legacy change: dry-run land exit $rc"; echo "$out" | sed 's/^/      /'; }

# An owner sign-off record (scripts/land/signoff.sh), published to origin. put-signoff <sha> <mode>
# [<other sha>]: valid; none removes it; othersha records <other sha> and files it under <sha>;
# tampered changes kimi.txt after recording; otherreview makes kimi's request body name <other sha>;
# otherreviewed gives that request a reviewed line for <other sha> while its body and output name
# <sha>; noreviewed removes its reviewed line; requestverdict puts ACCEPT only in agy's request;
# unnamedbody gives kimi's request no commit name; noagy drops agy's verdict; reject ends agy's output
# with VERDICT: REJECT; unnamed has a quote that names no commit.
printf '#!/usr/bin/env bash\nSIGNOFF=%q\n' "$ROOT_DIR/scripts/land/signoff.sh" > "$TMP/put-signoff"
cat >> "$TMP/put-signoff" <<'SIGNOFF_EOF'
set -e
sha="$(git rev-parse "$1")"; mode="$2"; R=refs/owner-signoff; tab="$(printf '\t')"
base="${SO_BASE:-$(git rev-parse origin/main)}"; rounds="${SO_ROUNDS:-5}"
git update-ref -d "$R/$sha" 2>/dev/null || true
CONCRETE_SKIP_GATES=1 git push -q origin ":$R/$sha" 2>/dev/null || true
[ "$mode" = none ] && exit 0
# The member's own review.sh note: the rounds it used (any verdict), which the cap is judged against.
bash "$(dirname "$0")/put-review" "$sha" "findings@$rounds"
d="$(mktemp -d)"
for r in codex kimi agy; do
  printf 'review of %s by %s\n' "$sha" "$r" > "$d/$r-request"
  printf '  VERDICT: ACCEPT\n' > "$d/$r"
done
bash "$SIGNOFF" record "$sha" --base "$base" --rounds-at "$sha" --date 2026-11-08 --quote "sign off ${sha:0:8}, yes" \
  --codex-request "$d/codex-request" --codex "$d/codex" \
  --kimi-request "$d/kimi-request" --kimi "$d/kimi" \
  --agy-request "$d/agy-request" --agy "$d/agy" >/dev/null
c="$(git rev-parse "$R/$sha")"
# variant SED [drop:FILE | set:FILE:BLOB]...: the valid record with record.txt edited by SED and files
# dropped or replaced (record writes only valid records, so every refused variant is built from one).
variant() {
  local rec entries t f b change
  rec="$(git cat-file blob "$c:record.txt" | sed "$1" | git hash-object -w --stdin)"
  entries="$(git ls-tree "$c" | grep -v "${tab}record.txt\$")"
  shift
  for change in "$@"; do
    case "$change" in
      drop:*) entries="$(printf '%s\n' "$entries" | grep -v "${tab}${change#drop:}\$")" ;;
      set:*) f="${change#set:}"; f="${f%%:*}"; b="${change##*:}"
        entries="$(printf '%s\n' "$entries" | sed "s/blob [0-9a-f]*${tab}${f}\$/blob ${b}${tab}${f}/")" ;;
    esac
  done
  t="$(printf '%s\n100644 blob %s\trecord.txt\n' "$entries" "$rec" | git mktree)"
  git update-ref "$R/$sha" "$(git commit-tree "$t" -m variant)"
}
set_request() { # reviewer stored-request-file
  local r="$1" q="$2"
  variant "s/^request-$r: .*/request-$r: $r-request.txt $(sha256sum < "$q" | cut -d' ' -f1)/" \
    "set:$r-request.txt:$(git hash-object -w "$q")"
}
case "$mode" in
  valid) ;;
  othersha) variant "s/^commit: .*/commit: $(git rev-parse "$3")/" ;;
  unnamed) variant "s/^quote: .*/quote: sign off the branch/" ;;
  tampered) variant "" "set:kimi.txt:$(printf 'edited output\n  VERDICT: ACCEPT\n' | git hash-object -w --stdin)" ;;
  otherreview) printf 'reviewed: %s\nreview of %s by kimi\n' "$sha" "$(git rev-parse "$3")" > "$d/kimi-o"
    set_request kimi "$d/kimi-o" ;;
  otherreviewed) printf 'reviewed: %s\nreview of %s by kimi\n' "$(git rev-parse "$3")" "$sha" > "$d/kimi-q"
    printf 'review of %s by kimi\nVERDICT: ACCEPT\n' "$sha" > "$d/kimi-o"
    variant "s/^request-kimi: .*/request-kimi: kimi-request.txt $(sha256sum < "$d/kimi-q" | cut -d' ' -f1)/;s/^verdict-kimi: .*/verdict-kimi: kimi.txt $(sha256sum < "$d/kimi-o" | cut -d' ' -f1)/" \
      "set:kimi-request.txt:$(git hash-object -w "$d/kimi-q")" "set:kimi.txt:$(git hash-object -w "$d/kimi-o")" ;;
  noreviewed) printf 'review of %s by kimi\n' "$sha" > "$d/kimi-q"; set_request kimi "$d/kimi-q" ;;
  unnamedbody) printf 'reviewed: %s\nreview the branch\n' "$sha" > "$d/kimi-q"; set_request kimi "$d/kimi-q" ;;
  requestverdict) printf 'reviewed: %s\nreview of %s by agy\nVERDICT: ACCEPT\n' "$sha" "$sha" > "$d/agy-q"
    printf 'agy output without a verdict\n' > "$d/agy-o"
    variant "s/^request-agy: .*/request-agy: agy-request.txt $(sha256sum < "$d/agy-q" | cut -d' ' -f1)/;s/^verdict-agy: .*/verdict-agy: agy.txt $(sha256sum < "$d/agy-o" | cut -d' ' -f1)/" \
      "set:agy-request.txt:$(git hash-object -w "$d/agy-q")" "set:agy.txt:$(git hash-object -w "$d/agy-o")" ;;
  noagy) variant "/^verdict-agy:/d" "drop:agy.txt" ;;
  reject) printf 'VERDICT: REJECT\n' > "$d/agy-r"
    variant "s/^verdict-agy: .*/verdict-agy: agy.txt $(sha256sum < "$d/agy-r" | cut -d' ' -f1)/" \
      "set:agy.txt:$(git hash-object -w "$d/agy-r")" ;;
  *) echo "put-signoff: unknown mode $mode" >&2; exit 2 ;;
esac
rm -rf "$d"
CONCRETE_SKIP_GATES=1 git push -q -f origin "$R/$sha"
SIGNOFF_EOF

echo "=== review: check fails closed, land requires review provenance ==="
git checkout -q main && git fetch -q origin && git reset -q --hard origin/main
MR="$(git rev-parse HEAD)"
git checkout -q -b brv1 "$MR"; printf 'rv1\n' > rv1.txt; commit_all brv1
git checkout -q -b brv2 "$MR"; printf 'rv2\n' > rv2.txt; commit_all brv2
bash "$TRAIN" build trv brv1 brv2 >/dev/null 2>&1 || no "review train build"
git checkout -q train/trv; HR="$(git rev-parse HEAD)"; R1="$(git rev-parse brv1)"; R2="$(git rev-parse brv2)"
out="$(LAND_REVIEW_CMD= bash "$TRAIN" check trv --no-remote 2>&1)"; rc=$?
[ "$rc" != 0 ] && has "$out" "no review command" && has "$out" "never skipped silently" \
  && ok "check with no review command fails instead of skipping the leg" || { no "missing reviewer: check exit $rc"; echo "$out" | sed 's/^/      /'; }
for m in none malformed blocked findings; do
  echo "$m" > "$FAKE_GH_DIR/review.mode"
  out="$(bash "$TRAIN" check trv --no-remote 2>&1)"; rc=$?
  [ "$rc" != 0 ] && has "$out" "review leg FAILED" \
    && ok "a reviewer that exits 0 leaving $( [ "$m" = none ] && echo 'no note' || echo "a $m note") fails check" || { no "reviewer $m: check exit $rc"; echo "$out" | sed 's/^/      /'; }
done
rm -f "$FAKE_GH_DIR/review.mode"
out="$(bash "$TRAIN" ci trv 2>&1)"; rc=$?
[ "$rc" != 0 ] && has "$out" "no passing check" && ok "a failed review leg leaves no pass for ci" || no "ci after a failed review leg: exit $rc"
# land: check --no-review passes the gates alone; land then judges the notes itself.
bash "$TMP/put-review" "$HR" none
bash "$TRAIN" check trv --no-remote --no-review >/dev/null 2>&1 || no "check --no-review"
printf '[{"databaseId": 6161, "headSha": "%s", "createdAt": "9999-01-01T00:00:00Z"}]' "$HR" > "$FAKE_GH_DIR/list.json"
bash "$TRAIN" ci trv >/dev/null 2>&1 || no "review train ci"
runjson "$HR" success success
prov_leg(){ # label head-mode m1-mode m2-mode expect(refuse|pass) [expected text]
  bash "$TMP/put-review" "$HR" "$2"; bash "$TMP/put-review" "$R1" "$3"; bash "$TMP/put-review" "$R2" "$4"
  local o r; o="$(bash "$TRAIN" --dry-run land trv 2>&1)"; r=$?
  if [ "$5" = refuse ]; then
    [ "$r" = 1 ] && has "$o" "has no review provenance" && has "$o" "${6:-}" && [ "$(git ls-remote origin refs/heads/main | cut -f1)" = "$MR" ] \
      && ok "land refuses $1" || { no "land with $1 (exit $r)"; echo "$o" | sed 's/^/      /'; }
  else
    [ "$r" = 0 ] && has "$o" "${6:-}" && ok "land accepts $1" || { no "land with $1 (exit $r)"; echo "$o" | sed 's/^/      /'; }
  fi
}
prov_leg "no review note anywhere" none none none refuse "member brv1:"
for m in malformed blocked findings; do
  prov_leg "an unreviewed head over a $m member note" none clean "$m" refuse "member brv2:"
  prov_leg "a $m head note, whatever the members" "$m" clean clean refuse "the head's review note does not qualify"
done
prov_leg "an unreviewed head with one member unreviewed" none clean none refuse "member brv2:"
prov_leg "an unreviewed head whose every member is clean at its exact SHA" none clean clean pass "every member (2) carries a clean one"
prov_leg "a clean head note" clean none none pass "the head ${HR:0:10} carries a clean review note"
# check over reviewed members takes the members' provenance and does not run the reviewer again.
bash "$TMP/put-review" "$HR" none; bash "$TMP/put-review" "$R1" clean; bash "$TMP/put-review" "$R2" clean
: > "$FAKE_GH_DIR/review.log"
out="$(bash "$TRAIN" check trv --no-remote 2>&1)"; rc=$?
[ "$rc" = 0 ] && [ ! -s "$FAKE_GH_DIR/review.log" ] \
  && ok "check over members reviewed clean passes the review leg without running the reviewer" || { no "check over reviewed members: exit $rc, reviewer calls '$(cat "$FAKE_GH_DIR/review.log")'"; echo "$out" | sed 's/^/      /'; }

echo "=== review: an owner sign-off at a member's exact SHA is its review provenance (R-0618) ==="
# brv1 is reviewed clean. brv2 used its rounds (a findings note, round 5: rv2.txt is in no class, so
# its cap is the highest, 5) and is signed off, or carries a record that must not count.
so_leg(){ # label mode expect(refuse|pass) text
  bash "$TMP/put-signoff" "$R2" "$2" "$R1"
  prov_leg "$1" none clean "$( [ "$2" = none ] && echo none || echo findings@5)" "$3" "$4"
}
so_leg "a member with a valid owner sign-off and a clean merge" valid pass "owner sign-off (1) at its exact SHA, and the head is their clean merge"
so_leg "a sign-off recorded for another commit" othersha refuse "the record is for $R1, not $R2"
so_leg "a sign-off whose verdict file was changed after recording" tampered refuse "the sha256 of kimi.txt is"
so_leg "a sign-off whose kimi request body names another commit" otherreview refuse "the kimi request body does not name ${R2:0:10}"
so_leg "F1: a reviewed line for another commit despite body and output naming the signed SHA" otherreviewed refuse "the kimi request's reviewed line is '$R1', not $R2"
so_leg "F1b: a request without a reviewed first line" noreviewed refuse "the kimi request's first line is not a 'reviewed: <full 40-hex SHA>' line"
so_leg "F2: ACCEPT in agy's request but no verdict in its output" requestverdict refuse "the agy output has no verdict"
so_leg "a request body that names no commit" unnamedbody refuse "the kimi request body does not name ${R2:0:10}"
so_leg "a sign-off without agy's verdict" noagy refuse "the record has no agy verdict"
so_leg "a sign-off whose agy output ends in REJECT" reject pass "owner sign-off (1) at its exact SHA, and the head is their clean merge"
o="$(bash "$ROOT_DIR/scripts/land/signoff.sh" verify "$R2" --base "$MR" 2>&1)"; r=$?
[ "$r" = 0 ] && has "$o" "verdicts: codex ACCEPT, kimi ACCEPT, agy REJECT" \
  && ok "verify accepts agy's REJECT at the cap and reports every verdict" || { no "verify with agy's REJECT (exit $r)"; echo "$o" | sed 's/^/      /'; }
so_leg "a sign-off whose quote names no commit" unnamed refuse "the quote does not name ${R2:0:10}"
so_leg "a member with neither a note nor a sign-off" none refuse "$R2 has no owner sign-off"
# The cap is the class of the member's diff: docs 2, scripts 3, compiler and spec 5, the highest wins.
git checkout -q -b bsodoc "$MR"; mkdir -p docs; printf 'd\n' > docs/so.md; commit_all bsodoc; SD="$(git rev-parse HEAD)"
git checkout -q -b bsoscr "$MR"; mkdir -p scripts; printf 'echo\n' > scripts/so.sh; commit_all bsoscr; SS="$(git rev-parse HEAD)"
git checkout -q -b bsomix "$MR"; mkdir -p docs Concrete; printf 'd\n' > docs/so.md; printf -- '-- x\n' > Concrete/So.lean; commit_all bsomix; SM="$(git rev-parse HEAD)"
o="$(SO_ROUNDS=2 bash "$TMP/put-signoff" "$SD" valid 2>&1 && bash "$ROOT_DIR/scripts/land/signoff.sh" verify "$SD" --base "$MR" 2>&1)"; r=$?
[ "$r" = 0 ] && has "$o" "docs change, cap 2 reached" && ok "a docs-only member signed off after 2 rounds is accepted" || { no "docs member after 2 rounds (exit $r)"; echo "$o" | sed 's/^/      /'; }
for c in "SS scripts 3" "SM compiler 5"; do
  read -r sv cls cap <<< "$c"; s="${!sv}"
  o="$(SO_ROUNDS=2 bash "$TMP/put-signoff" "$s" valid 2>&1)"; r=$?
  [ "$r" != 0 ] && has "$o" "the review cap was not reached: a $cls change has cap $cap, and ${s:0:10} records 2 round(s)" && ! git rev-parse -q --verify "refs/owner-signoff/$s" >/dev/null \
    && ok "record refuses a $cls member after 2 rounds and writes nothing" || { no "record of a $cls member after 2 rounds (exit $r)"; echo "$o" | sed 's/^/      /'; }
  SO_ROUNDS="$cap" bash "$TMP/put-signoff" "$s" valid >/dev/null 2>&1 || no "record of a $cls member at its cap $cap"
  bash "$TMP/put-review" "$s" findings@2
  o="$(bash "$ROOT_DIR/scripts/land/signoff.sh" verify "$s" --base "$MR" 2>&1)"; r=$?
  [ "$r" = 1 ] && has "$o" "the review cap was not reached: a $cls change has cap $cap, and ${s:0:10} records 2 round(s)" \
    && ok "verify refuses a $cls member's sign-off after 2 rounds" || { no "verify of a $cls member after 2 rounds (exit $r)"; echo "$o" | sed 's/^/      /'; }
  bash "$TMP/put-signoff" "$s" none
done
# The cap evidence is a well-formed review.sh note: a bare `rounds: 5` note does not count.
bash "$TMP/put-signoff" "$R2" valid
git notes --ref=refs/notes/concrete-review add -f -m "rounds: 5" "$R2"
o="$(bash "$ROOT_DIR/scripts/land/signoff.sh" verify "$R2" --base "$MR" 2>&1)"; r=$?
[ "$r" = 1 ] && has "$o" "rounds-at ${R2:0:10} carries no well-formed review.sh note" \
  && ok "verify refuses cap evidence that is not a well-formed review.sh note" || { no "malformed cap note (exit $r)"; echo "$o" | sed 's/^/      /'; }
bash "$TMP/put-review" "$R2" none; bash "$TMP/put-signoff" "$R2" none
bash "$TMP/put-signoff" "$SD" none
git checkout -q train/trv
# The same valid sign-off, but a head beyond the clean merge of its members. train.sh already refuses
# a head that is not the built train; pure_merge is the provenance's own check, run here on heads that
# add a commit, or that change a merge's tree, over the built train.
bash "$TMP/put-signoff" "$R2" valid
BR="$MR"   # the train was built on main at MR
git checkout -q --detach "$HR"; printf 'extra\n' > extra.txt; git add extra.txt; git commit -qm extra; HX="$(git rev-parse HEAD)"
git checkout -q --detach "$HR"; printf 'extra\n' > extra.txt; git add extra.txt; git commit -q --amend --no-edit; HY="$(git rev-parse HEAD)"
git checkout -q train/trv
pm(){ bash -c 'eval "$(sed -n "/^pure_merge() {/,/^}/p" "$1")"; pure_merge "$2" "$3" "$4" "$5"' _ "$TRAIN" "$BR" "$1" "$R1" "$R2"; }
o="$(pm "$HR")"; [ -z "$o" ] && ok "pure_merge: the built train is the clean merge of its members" || no "pure_merge refused the built train: '$o'"
o="$(pm "$HX")"; has "$o" "${HX:0:10} is not the clean merge of its members onto ${BR:0:10}: ${HX:0:10} is not the merge of ${R2:0:10}" \
  && ok "pure_merge refuses a head with an extra commit ($o)" || no "pure_merge on an extra commit: '$o'"
o="$(pm "$HY")"; has "$o" "${HY:0:10} is not the clean merge of its members onto ${BR:0:10}: ${HY:0:10} differs from the clean merge of ${R2:0:10}" \
  && ok "pure_merge refuses a merge whose tree carries an extra change ($o)" || no "pure_merge on a changed merge: '$o'"
bash "$TMP/put-signoff" "$R2" none

echo "=== where a train runs: a full clone with the repository hook (ISS-0015) ==="
cd "$W" || exit 2
git checkout -q -f main
git worktree add -q --detach "$TMP/linked-wt" main
out="$(cd "$TMP/linked-wt" && bash "$TRAIN" build twt --no-fetch b1 2>&1)"; rc=$?
[ "$rc" != 0 ] && has "$out" "refusing to run in a linked worktree" && [ ! -e "$TMP/state/twt" ] \
  && ok "train.sh refuses a linked worktree before doing anything" || { no "train in a linked worktree: exit $rc"; echo "$out" | sed 's/^/      /'; }
out="$(cd "$TMP/linked-wt" && bash "$TRAIN" status 2>&1)"; rc=$?
! has "$out" "refusing" && ok "train.sh status (read-only) still runs in a linked worktree" || no "status refused in a worktree: $out"
hook="$ROOT_DIR/.githooks/pre-push"
Z=0000000000000000000000000000000000000000
mh="$(git rev-parse main)"
out="$(cd "$TMP/linked-wt" && printf 'refs/heads/main %s refs/heads/ci/twt %s\n' "$mh" "$Z" | CONCRETE_GATES_SHELL= bash "$hook" origin "$TMP/origin.git" 2>&1)"; rc=$?
[ "$rc" = 1 ] && has "$out" "is a linked worktree; push it from a full clone" \
  && ok "the pre-push hook refuses a gated push from a linked worktree (before any check runs)" || { no "hook in a linked worktree: exit $rc"; echo "$out" | sed 's/^/      /'; }
out="$(cd "$TMP/linked-wt" && printf 'refs/heads/x %s refs/heads/wip/twt %s\n' "$mh" "$Z" | bash "$hook" origin "$TMP/origin.git" 2>&1)"; rc=$?
[ "$rc" = 0 ] && has "$out" "wip/* backup push" && ok "a wip/* backup push from a linked worktree still passes (it runs nothing)" || { no "wip push from a worktree: exit $rc"; echo "$out" | sed 's/^/      /'; }
git worktree remove --force "$TMP/linked-wt"
# hook off: not executable, missing, or some other hook that does not run the repository's.
chmod -x "$TMP/nohooks/pre-push"
out="$(bash "$TRAIN" record "$(git rev-parse main)" --base "$(git rev-parse main)" 2>&1)"
! has "$out" "refusing to run" && ok "train.sh record (CI's Landing record job: no hook there) is not refused for the hook" || { no "record refused without a hook"; echo "$out" | sed 's/^/      /'; }
out="$(bash "$TRAIN" build tnh --no-fetch b1 2>&1)"; rc=$?
[ "$rc" != 0 ] && has "$out" "no enabled pre-push hook" && ok "train.sh refuses when the pre-push hook is not executable" || { no "train with a disabled hook: exit $rc"; echo "$out" | sed 's/^/      /'; }
mkdir -p "$TMP/empty-hooks"
out="$(GIT_CONFIG_COUNT=1 GIT_CONFIG_KEY_0=core.hooksPath GIT_CONFIG_VALUE_0="$TMP/empty-hooks" bash "$TRAIN" build tnh --no-fetch b1 2>&1)"; rc=$?
[ "$rc" != 0 ] && has "$out" "no enabled pre-push hook" && ok "train.sh refuses when core.hooksPath names a directory with no pre-push" || { no "train with an empty hooks dir: exit $rc"; echo "$out" | sed 's/^/      /'; }
printf '#!/usr/bin/env bash\nexit 0\n' > "$TMP/nohooks/pre-push"; chmod +x "$TMP/nohooks/pre-push"
out="$(bash "$TRAIN" build tnh --no-fetch b1 2>&1)"; rc=$?
[ "$rc" != 0 ] && has "$out" "is not the repository's .githooks/pre-push" && ok "train.sh refuses a pre-push hook that does not run the repository's" || { no "train with a foreign hook: exit $rc"; echo "$out" | sed 's/^/      /'; }
printf '#!/usr/bin/env bash\n# runs .githooks/pre-push (it does not)\nexit 0\n' > "$TMP/nohooks/pre-push"
out="$(bash "$TRAIN" build tnh --no-fetch b1 2>&1)"; rc=$?
[ "$rc" != 0 ] && has "$out" "is not the repository's .githooks/pre-push" && ok "train.sh refuses a hook that only mentions .githooks/pre-push" || { no "train with a hook naming the file: exit $rc"; echo "$out" | sed 's/^/      /'; }
out="$(cd "$W" && bash "$ROOT_DIR/.githooks/pre-push" --concrete-identify < /dev/null 2>&1)"; rc=$?
[ "$rc" = 0 ] && [ "$out" = "concrete-pre-push $(cd "$ROOT_DIR" && pwd -P)/.githooks/pre-push" ] \
  && ok "the repository hook answers who it is with its real path and runs nothing" || { no "hook identify: exit $rc, '$out'"; }
# (the wip tier, so the hook runs nothing heavy and never re-enters these checks)
out="$(cd "$W" && printf 'refs/heads/x %s refs/heads/wip/idn %s\n' "$(git rev-parse main)" 0000000000000000000000000000000000000000 \
  | CONCRETE_PREPUSH_IDENTIFY=1 bash "$ROOT_DIR/.githooks/pre-push" origin "$TMP/origin.git" 2>&1)"; rc=$?
! has "$out" "concrete-pre-push /" && has "$out" "wip/* backup push" \
  && ok "a real push never takes the identify answer (no environment switch exists)" || { no "identify leaked into a push: $out"; }
standin_hook
[ ! -e "$TMP/state/tnh" ] && ok "no refused run left train state behind" || no "a refused run left state"

# The regression suite drives real index stages, reverse-order additions, conflicting IDs,
# malformed rows, and the issues gate on the canonical resolved output.
if bash "$ROOT_DIR/scripts/checks/lib/bounded.sh" 180 python3 -I "$ROOT_DIR/scripts/ids/round3_controls.py"; then
  ok "issue merge and reservation regressions"
else no "issue merge and reservation regressions"; fi

echo
echo "check_landing_script: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
