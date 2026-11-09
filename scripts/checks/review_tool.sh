#!/usr/bin/env bash
# R-0544 — the review tool (scripts/review/review.sh, verify.sh, split_diff.py)
# does what docs/REVIEW.md says it does.
#
# No API key or network is needed: fake Codex, Kimi and agy reviewers answer according to MARK_*
# markers or clean, and log every call (prompt kind, closed stdin, files, archived tree) so the legs
# can check what was sent. All git work happens in throwaway repositories with a bare repository as
# `origin`, so the notes push, including the non-fast-forward path, runs for real.
#
# Legs: verdict and repository-access parsing, exact prompt persistence and hashes, both prompts'
# permanent owner rule, independent reviewer/writer selection and refusal, note writing and its
# format, notes push and the non-fast-forward merge, delta selection (round 2 sees only the delta
# and previous findings), splitting (file groups, size limit, hunk split, --pieces N), the
# content-filter refusal retry (once, neutral, smaller, then `blocked`), the round cap (distinct
# exit code, no extra round), verify.sh including legacy clean-note and required-base refusal, and
# the pre-push hook's review-notes tier (a notes-only push runs no gates).

set -uo pipefail
source "$(dirname "${BASH_SOURCE[0]}")/lib/gate_env.sh" || exit 2
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
REVIEW="$ROOT_DIR/scripts/review/review.sh"
VERIFY="$ROOT_DIR/scripts/review/verify.sh"
SPLIT="$ROOT_DIR/scripts/review/split_diff.py"

PASS=0
FAIL=0
ok() { echo "  ok   $1"; PASS=$((PASS+1)); }
no() { echo "  FAIL $1"; FAIL=$((FAIL+1)); }
check() { # check <label> <command...>: the command must succeed
  local label="$1"; shift
  if "$@"; then ok "$label"; else no "$label"; fi
}

T="$(mktemp -d)"
trap 'rm -rf "$T"' EXIT

# Isolate git from the caller's repository (a hook exports GIT_DIR; ISS-0015) and from the user's
# config (hooks, signing, identity).
source "$ROOT_DIR/scripts/checks/lib/git_sandbox.sh" || exit 2
git_sandbox "$T/gitconfig" || exit 2
cat >"$GIT_CONFIG_GLOBAL" <<'EOF'
[user]
	name = Review Gate
	email = review-gate@example.invalid
[init]
	defaultBranch = main
[advice]
	detachedHead = false
EOF

snapshot_source_tree() {
  local target_dir="${1:-$ROOT_DIR}"
  python3 -B -I - "$target_dir" <<'PY'
import hashlib, os, stat, sys
sys.dont_write_bytecode = True

def raise_err(err):
    raise err

root = os.path.abspath(sys.argv[1])
if not os.path.isdir(root) or os.path.islink(root):
    sys.exit(f"snapshot_source_tree: '{root}' is not a directory")

entries = []
for dirpath, dirnames, filenames in os.walk(root, topdown=True, onerror=raise_err, followlinks=False):
    rel_dir = os.path.relpath(dirpath, root)
    if rel_dir == "." and ".git" in dirnames:
        dirnames.remove(".git")
    dir_st = os.lstat(dirpath)
    entries.append(f"D\t{rel_dir}\t{stat.S_IMODE(dir_st.st_mode):04o}")
    real_dirs = []
    for name in sorted(dirnames + filenames):
        p = os.path.join(dirpath, name)
        rel = os.path.relpath(p, root)
        st = os.lstat(p)
        mode = stat.S_IMODE(st.st_mode)
        if stat.S_ISLNK(st.st_mode):
            target = os.readlink(p)
            th = hashlib.sha256(os.fsencode(target)).hexdigest()
            entries.append(f"L\t{rel}\t{mode:04o}\t{th}\t{target}")
        elif stat.S_ISDIR(st.st_mode):
            real_dirs.append(name)
        elif stat.S_ISREG(st.st_mode):
            h = hashlib.sha256()
            size = 0
            with open(p, "rb") as f:
                while True:
                    chunk = f.read(65536)
                    if not chunk:
                        break
                    size += len(chunk)
                    h.update(chunk)
            entries.append(f"F\t{rel}\t{mode:04o}\t{size}\t{h.hexdigest()}")
        else:
            sys.exit(f"snapshot_source_tree: unsupported file type at '{rel}'")
    dirnames[:] = real_dirs

if not entries:
    sys.exit("snapshot_source_tree: empty snapshot")
sys.stdout.write("\n".join(entries) + "\n")
PY
}

verify_source_tree_unchanged() { # verify_source_tree_unchanged <before-file> <before-rc> <target-dir> <after-file>
  local before_file="$1" before_rc="$2" target_dir="$3" after_file="$4" after_rc=0
  [ "$before_rc" -eq 0 ] && [ -s "$before_file" ] || return 1
  snapshot_source_tree "$target_dir" >"$after_file" 2>/dev/null || after_rc=$?
  [ "$after_rc" -eq 0 ] && [ -s "$after_file" ] || return 1
  cmp -s "$before_file" "$after_file"
}

verify_source_tree_changed() { # verify_source_tree_changed <before-file> <before-rc> <target-dir> <after-file>
  ! verify_source_tree_unchanged "$@"
}

ROOT_SNAP_BEFORE_RC=0
snapshot_source_tree "$ROOT_DIR" >"$T/root.before" || ROOT_SNAP_BEFORE_RC=$?

# ------------------------------------------------------------------ fake codex
FAKE_LOG="$T/fake"
mkdir -p "$FAKE_LOG" "$T/bin"
export FAKE_LOG
# Bug 209's legs prove review.sh OVERRIDES the caller's setting: the gate runs with optional locks
# explicitly on, so a value inherited from whoever launched it cannot satisfy them.
export GIT_OPTIONAL_LOCKS=1
cat >"$T/bin/codex" <<'EOF'
#!/usr/bin/env bash
# Fake reviewer: `codex exec -s danger-full-access ... -o <file> <prompt>`, diff in the prompt.
set -uo pipefail
[ "${1:-}" = exec ] || { echo "fake codex: expected exec" >&2; exit 64; }
shift
out="" prompt="" sandbox="" cd="" skip=0
while [ $# -gt 0 ]; do
  case "$1" in
    -o|--output-last-message) out="$2"; shift 2 ;;
    -s|--sandbox) sandbox="$2"; shift 2 ;;
    -C|--cd) cd="$2"; shift 2 ;;
    --color|-m|--model|-c) shift 2 ;;
    --skip-git-repo-check) skip=1; shift ;;
    --ephemeral) shift ;;
    *) prompt="$1"; shift ;;
  esac
done
[ -z "$cd" ] || cd "$cd" || exit 65
n=$(( $(ls "$FAKE_LOG" | grep -c '^stdin\.' || true) + 1 ))
cat >"$FAKE_LOG/actual-stdin.$n"
printf '%s' "$prompt" >"$FAKE_LOG/prompt.$n"
awk 'found { print } /^--- supplied diff: / { found=1 }' "$FAKE_LOG/prompt.$n" >"$FAKE_LOG/stdin.$n"
kind=standard
grep -q "routine code review" "$FAKE_LOG/prompt.$n" && kind=neutral
files="$(grep '^diff --git ' "$FAKE_LOG/stdin.$n" | sed 's#.* b/##' | sort -u | tr '\n' ',')"
archive=no
[ ! -e .git ] && [ "$(cat README.md 2>/dev/null)" = base ] && [ ! -w README.md ] && [ ! -w . ] && archive=yes
echo "$n $kind sandbox=$sandbox bytes=$(wc -c <"$FAKE_LOG/stdin.$n" | tr -d ' ') archive=$archive skip_git_check=$skip stdin_bytes=$(wc -c <"$FAKE_LOG/actual-stdin.$n" | tr -d ' ') files=$files" >>"$FAKE_LOG/calls"
echo "$n optional_locks=${GIT_OPTIONAL_LOCKS:-unset}" >>"$FAKE_LOG/locks"
added="$(grep '^+' "$FAKE_LOG/stdin.$n" || true)"
has() { grep -q "$1" <<<"$added"; }
# Real `codex exec` writes a session transcript ending with a "tokens used" trailer to stdout, while
# writing the agent's final message only to `-o $out`. Emulate that in fake codex so any collector
# that reads stdout instead of `-o` fails the strict output contract.
reply() { printf '%s\n' "$1" >"$out"; printf '%s\ntokens used\n1,234\n' "$1"; }
if has MARK_REFUSE_ALWAYS || { has MARK_REFUSE_STANDARD && [ "$kind" = standard ]; }; then
  reply "I'm sorry, but I can't help with that request because it was flagged by our cybersecurity policy."
elif has MARK_DOUBLE_TIMEOUT; then
  sleep 1; exit 0
elif has MARK_ERROR; then
  echo "stream error: 500" >&2; exit 1
elif has MARK_MUTATE_TREE; then
  chmod u+w README.md 2>/dev/null && printf 'mutated\n' > README.md
  reply "READ: README.md
REPOSITORY: readable
VERDICT: clean"
elif has MARK_REPOSITORY_UNREADABLE; then
  reply "READ: README.md
REPOSITORY: unreadable
VERDICT: clean"
elif has MARK_SANDBOX_FAILURE; then
  reply "READ: README.md
sandbox could not start
REPOSITORY: readable
VERDICT: clean"
elif has MARK_NO_REPOSITORY; then
  reply "READ: README.md
Nothing to report.
VERDICT: clean"
elif has MARK_REPO_WRONG_POS; then
  reply "READ: README.md
REPOSITORY: readable
One more remark after REPOSITORY.
VERDICT: clean"
elif has MARK_NO_READ; then
  reply "Nothing to report.
REPOSITORY: readable
VERDICT: clean"
elif has MARK_BAD_READ; then
  reply "READ: does-not-exist-in-tree.txt
REPOSITORY: readable
VERDICT: clean"
elif has MARK_READ_DIR; then
  reply "READ: p
REPOSITORY: readable
VERDICT: clean"
elif has MARK_READ_ABS; then
  reply "READ: /README.md
REPOSITORY: readable
VERDICT: clean"
elif has MARK_FINDING; then
  reply "- a.txt:1: the counter is never reset
READ: README.md
REPOSITORY: readable
VERDICT: findings"
elif has MARK_NOVERDICT; then
  reply "READ: README.md
REPOSITORY: readable
I looked at it and it seems fine."
elif has MARK_TRAILING; then
  reply "READ: README.md
REPOSITORY: readable
VERDICT: clean
Also, one more thought."
elif has MARK_BOLD; then
  reply "Nothing to report.
**READ: README.md**
**REPOSITORY: readable**
**VERDICT: clean**"
elif has MARK_MENTION_FILTER; then
  reply "The content filter detection and the cybersecurity wording look right.
READ: README.md
REPOSITORY: readable
VERDICT: clean"
else
  reply "Nothing to report.
READ: README.md
REPOSITORY: readable
VERDICT: clean"
fi
EOF
chmod +x "$T/bin/codex"
export REVIEW_CODEX_BIN="$T/bin/codex"

# Kimi and agy have one-argument prompt interfaces and answer on stdout. Their fakes pin the exact
# permission flags, archived cwd and closed stdin; the production result parser is the Codex one.
for reviewer in kimi agy; do
  cat >"$T/bin/$reviewer" <<'EOF'
#!/usr/bin/env bash
set -uo pipefail
reviewer="$(basename "$0")"
if [ "$reviewer" = agy ]; then
  [ "${1:-}" = --dangerously-skip-permissions ] || { echo "fake agy: missing permission flag" >&2; exit 64; }
  shift
fi
[ "${1:-}" = -p ] && [ $# -eq 2 ] || { echo "fake $reviewer: expected -p PROMPT" >&2; exit 64; }
prompt="$2"
n=$(( $(ls "$FAKE_LOG" | grep -c '^stdin\.' || true) + 1 ))
cat >"$FAKE_LOG/actual-stdin.$n"
printf '%s' "$prompt" >"$FAKE_LOG/prompt.$n"
awk 'found { print } /^--- supplied diff: / { found=1 }' "$FAKE_LOG/prompt.$n" >"$FAKE_LOG/stdin.$n"
archive=no
[ ! -e .git ] && [ "$(cat README.md 2>/dev/null)" = base ] && [ ! -w README.md ] && [ ! -w . ] && archive=yes
echo "$n $reviewer archive=$archive stdin_bytes=$(wc -c <"$FAKE_LOG/actual-stdin.$n" | tr -d ' ')" >>"$FAKE_LOG/calls"
echo "$n optional_locks=${GIT_OPTIONAL_LOCKS:-unset}" >>"$FAKE_LOG/locks"
case "$prompt" in
  *MARK_REFUSE_ALWAYS*) printf "I'm sorry, but I can't help with that request because it was flagged by our cybersecurity policy.\n" ;;
  *MARK_DOUBLE_TIMEOUT*) sleep 1; exit 0 ;;
  *MARK_ERROR*) echo "stream error: 500" >&2; exit 1 ;;
  *MARK_REPOSITORY_UNREADABLE*) printf 'READ: README.md\nREPOSITORY: unreadable\nVERDICT: clean\n' ;;
  *MARK_SANDBOX_FAILURE*) printf 'READ: README.md\nbwrap: operation not permitted\nREPOSITORY: readable\nVERDICT: clean\n' ;;
  *MARK_NO_REPOSITORY*) printf 'READ: README.md\nNothing to report.\nVERDICT: clean\n' ;;
  *MARK_REPO_WRONG_POS*) printf 'READ: README.md\nREPOSITORY: readable\nRemark after REPOSITORY.\nVERDICT: clean\n' ;;
  *MARK_NO_READ*) printf 'Nothing to report.\nREPOSITORY: readable\nVERDICT: clean\n' ;;
  *MARK_BAD_READ*) printf 'READ: missing-file.txt\nREPOSITORY: readable\nVERDICT: clean\n' ;;
  *MARK_FINDING*) printf -- '- a.txt:1: the counter is never reset\nREAD: README.md\nREPOSITORY: readable\nVERDICT: findings\n' ;;
  *MARK_NOVERDICT*) printf 'READ: README.md\nREPOSITORY: readable\nI looked at it and it seems fine.\n' ;;
  *MARK_TRAILING*) printf 'READ: README.md\nREPOSITORY: readable\nVERDICT: clean\nTrailing line.\n' ;;
  *) printf 'Nothing to report.\nREAD: README.md\nREPOSITORY: readable\nVERDICT: clean\n' ;;
esac
EOF
  chmod +x "$T/bin/$reviewer"
done
export REVIEW_KIMI_BIN="$T/bin/kimi" REVIEW_AGY_BIN="$T/bin/agy"

calls() { [ -f "$FAKE_LOG/calls" ] && wc -l <"$FAKE_LOG/calls" | tr -d ' ' || echo 0; }
reset_fake() { rm -f "$FAKE_LOG"/*; }

# ------------------------------------------------------------------ repos & signing keys
mkdir -p "$T/keys" "$T/ev"
ssh-keygen -q -t ed25519 -N "" -C "orchestrator@concrete" -f "$T/keys/id_ed25519"
ssh-keygen -q -t ed25519 -N "" -C "attacker@concrete" -f "$T/keys/unauth_ed25519"
printf 'orchestrator@concrete namespaces="git" %s\n' "$(cat "$T/keys/id_ed25519.pub")" >"$T/keys/allowed_signers"
export REVIEW_WRITER_SIGNING_KEY="$T/keys/id_ed25519"
export REVIEW_WRITER_SIGNER="orchestrator@concrete"
export REVIEW_WRITER_ALLOWED_SIGNERS="$T/keys/allowed_signers"

sign_raw_stanza() { # sign_raw_stanza <in-file> <out-file> [key-file] [signer]
  python3 -B -I - "$ROOT_DIR/scripts/review/writer_provenance.py" "$1" "$2" "${3:-$T/keys/id_ed25519}" "${4:-orchestrator@concrete}" <<'PY'
import importlib.util, re, sys
sys.dont_write_bytecode = True
spec = importlib.util.spec_from_file_location("wp", sys.argv[1])
wp = importlib.util.module_from_spec(spec)
spec.loader.exec_module(wp)
lines = [ln for ln in open(sys.argv[2], encoding="utf-8").read().splitlines() if ln.strip()]
entry = {}
for ln in lines:
    if ln == wp.SIG_BEGIN:
        break
    m = re.match(r"^([a-z0-9-]+): (.*)$", ln)
    if m and m.group(1) not in ("entry-id", "signer"):
        entry[m.group(1)] = m.group(2)
entry.setdefault("source", "harness")
signed = wp.sign_entry(entry, sys.argv[4], sys.argv[5])
with open(sys.argv[3], "w", encoding="utf-8") as f:
    f.write(wp.format_log_stanza(signed) + "\n")
PY
}

git init -q --bare "$T/origin.git"
git clone -q "$T/origin.git" "$T/w" 2>/dev/null
W="$T/w"
g() { git -C "$W" "$@"; }
commit_unstamped() { # commit_unstamped <path> <content> <message>
  mkdir -p "$(dirname "$W/$1")"
  printf '%s\n' "$2" >"$W/$1"
  g add -A && g commit -qm "$3"
}
stamp_writer() { # stamp_writer <model> [round] [branch]
  local m="$1" r="${2:-1}" b="${3:-$(g rev-parse --abbrev-ref HEAD)}" extra=()
  [ "$m" = agy ] && extra=(--agy-diff-sha256 "1111111111111111111111111111111111111111111111111111111111111111")
  python3 -I "$ROOT_DIR/scripts/review/writer_provenance.py" stamp-commit --repo "$W" \
    --model "$m" --round "$r" --branch "$b" \
    --brief-sha256 "2222222222222222222222222222222222222222222222222222222222222222" \
    "${extra[@]}" >/dev/null
}
backfill_writer() { # backfill_writer <commit> <model> <round> <branch> [start-sha] [end-sha] [evidence] [supersedes] [reason]
  local c; c="$(g rev-parse "$1")"
  local m="$2" r="$3" b="$4"
  local s="${5:-$(g rev-parse "$c~1")}"
  local e="${6:-$c}"
  local ev="${7:-audited session transcript for $b round $r ($m)}"
  local sup="${8:-}" rsn="${9:-}" sup_args=()
  [ -n "$sup" ] && sup_args=(--supersedes "$sup" --reason "$rsn")
  printf 'brief for %s round %s (%s)\n' "$b" "$r" "$m" >"$T/ev/$c.brief"
  printf 'raw output log for %s round %s (%s) on %s\n' "$b" "$r" "$m" "$c" >"$T/ev/$c.output"
  python3 -I "$ROOT_DIR/scripts/review/writer_provenance.py" backfill --repo "$W" \
    --commit "$c" --model "$m" --round "$r" --branch "$b" \
    --brief-file "$T/ev/$c.brief" \
    --output-file "$T/ev/$c.output" \
    --start-sha "$s" --end-sha "$e" --evidence "$ev" "${sup_args[@]}" >/dev/null
}
commit_file() { # commit_file <path> <content> <message> — stamps as claude explicitly
  commit_unstamped "$1" "$2" "$3"
  stamp_writer claude 1 "$(g rev-parse --abbrev-ref HEAD)"
}
commit_unstamped README.md "base" "base"
printf '.review/\n' >"$W/.gitignore"
g add .gitignore && g commit -qm "ignore .review"
g push -q origin main
review() { (cd "$W" && bash "$REVIEW" ${REVIEW_TEST_WRITER:+--writer "$REVIEW_TEST_WRITER"} "$@"); }
note() { g notes --ref=refs/notes/concrete-review show "${1:-HEAD}" 2>/dev/null; }
field() { note "${2:-HEAD}" | sed -n "s/^$1: //p"; }
sha256_file() { if command -v sha256sum >/dev/null 2>&1; then sha256sum "$1" | cut -d' ' -f1; else shasum -a 256 "$1" | cut -d' ' -f1; fi; }

echo "=== round 1: full diff, findings, note written and pushed ==="
g checkout -qb feature
commit_file old.txt "MARK_FINDING in the first change" "first change"
review >"$T/out1" 2>&1; rc=$?
check "findings exit 1 (got $rc)" test "$rc" -eq 1
check "one reviewer call" test "$(calls)" -eq 1
check "reviewer has full access only to a read-only archived tree, with stdin closed" \
  grep -q "sandbox=danger-full-access .*archive=yes skip_git_check=1 stdin_bytes=0" "$FAKE_LOG/calls"
# Bug 209: every reviewer call runs without git's optional locks, so a reviewer command stopped
# mid-`git status` cannot leave the caller's worktree with a stale index.lock.
check "reviewer runs with GIT_OPTIONAL_LOCKS=0" grep -q " optional_locks=0$" "$FAKE_LOG/locks"
check "no reviewer call ran with optional locks" bash -c '! grep -v " optional_locks=0$" "$1" | has_match .' _ "$FAKE_LOG/locks"
check "round 1 prompt carries the verdict rule" grep -q "^VERDICT: findings$" "$FAKE_LOG/prompt.1"
check "round 1 prompt requires READ: <path> evidence" grep -q "READ: <path>" "$FAKE_LOG/prompt.1"
check "round 1 prompt requires repository readability evidence" grep -q "REPOSITORY: readable.*REPOSITORY: unreadable" "$FAKE_LOG/prompt.1"
check "the standard prompt carries the permanent owner rule" \
  grep -Fq 'OWNER RULE (2026-11-08, verbatim: "ALWAYS tell the reviewrs to do the best long term thing rather than cuitting corners"): judge and fix by the best long-term design. Never cut corners for speed, a smaller diff or fewer rounds. As a reviewer, report any shortcut, containment patch or corner cut as a real finding and say what the long-term fix is. As a writer, implement the structural fix, not a containment patch; if staging is truly unavoidable, record the long-term design and name its roadmap row.' "$FAKE_LOG/prompt.1"
check "round 1 reviews the full diff from the merge-base" grep -q "files=old.txt,$" "$FAKE_LOG/calls"
R1="$(g rev-parse HEAD)"
BASE="$(g merge-base HEAD origin/main)"
FORMAT_RE='^verdict: (clean|findings|blocked)
rounds: [0-9]+
reviewer: codex
writer: claude
utc: [0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}Z
base: [0-9a-f]{40}
pieces: [0-9]+$'
n1="$(note "$R1")"
if [[ "$n1" =~ $FORMAT_RE ]]; then ok "note has exactly the seven common fields, in order"; else no "note format: $n1"; fi
check "note: verdict findings" test "$(field verdict)" = findings
check "note: rounds 1" test "$(field rounds)" = 1
check "note: base is the merge-base" test "$(field base)" = "$BASE"
check "note: pieces 1" test "$(field pieces)" = 1
check "raw output at .review/<sha>-1.md" test -s "$W/.review/$R1-1.md"
check "findings recorded under the findings ref" \
  bash -c "git -C '$W' notes --ref=refs/notes/concrete-review-findings show $R1 | has_match 'counter is never reset'"
check "notes ref pushed to origin" test "$(git -C "$T/origin.git" rev-parse refs/notes/concrete-review)" = "$(g rev-parse refs/notes/concrete-review)"
check ".review/ stays untracked" test -z "$(g status --porcelain)"
(cd "$W" && bash "$VERIFY" HEAD >/dev/null); rc=$?
check "verify.sh: findings is not clean (exit 1, got $rc)" test "$rc" -eq 1

echo "=== same HEAD again: nothing to review ==="
review >/dev/null 2>&1; rc=$?
check "re-review of a reviewed HEAD exits 4 (got $rc)" test "$rc" -eq 4
check "and calls no reviewer" test "$(calls)" -eq 1

echo "=== --head: the callers' contract (train.sh, ready.sh pass --base --head) ==="
review --head HEAD >/dev/null 2>&1; rc=$?
check "--head at the checkout's HEAD is accepted (reviewed HEAD exits 4, got $rc)" test "$rc" -eq 4
review --base main --head "$(g rev-parse HEAD)" >/dev/null 2>&1; rc=$?
check "--base --head <sha> as train.sh calls it is accepted (got $rc)" test "$rc" -eq 4
review --head main >"$T/out-head" 2>&1; rc=$?
check "--head other than the checkout refuses (exit 2, got $rc)" test "$rc" -eq 2
check "and says why" grep -q "but the checkout is at" "$T/out-head"
review --head "" >/dev/null 2>&1; rc=$?
check "an empty --head refuses (exit 2, got $rc)" test "$rc" -eq 2
check "and calls no reviewer" test "$(calls)" -eq 1

echo "=== round 2: only the delta, with the previous findings ==="
commit_file old.txt "fixed" "fix the counter"
commit_file new.txt "a second change" "second change"
review >"$T/out2" 2>&1; rc=$?
check "clean exit 0 (got $rc)" test "$rc" -eq 0
check "round 2 diff holds only the delta (new.txt, old.txt fix)" grep -q "^2 standard .*files=new.txt,old.txt,$" "$FAKE_LOG/calls"
check "round 2 diff does not resend the round-1 hunk" bash -c "! grep -q '^+MARK_FINDING' '$FAKE_LOG/stdin.2'"
check "round 2 prompt names the delta start" grep -q "change since commit $R1" "$FAKE_LOG/prompt.2"
check "round 2 prompt carries the previous findings" grep -q "counter is never reset" "$FAKE_LOG/prompt.2"
check "note: rounds 2, verdict clean" test "$(field rounds)/$(field verdict)" = "2/clean"
check "clean note records repository: readable" test "$(field repository)" = readable
R2="$(g rev-parse HEAD)"
P2="$W/.review/$R2-2.prompt.1.txt"
check "the exact prompt is saved next to the raw output" cmp -s "$P2" "$FAKE_LOG/prompt.2"
P2_SHA="$(sha256_file "$P2")"
check "the raw output records the saved prompt's sha256" \
  bash -c "sed -n 's/^prompt sha256: //p' '$W/.review/$R2-2.md' | has_match -x '$P2_SHA'"
(cd "$W" && bash "$VERIFY" HEAD >"$T/v" 2>&1); rc=$?
check "verify.sh: clean exit 0 (got $rc)" test "$rc" -eq 0
check "verify.sh prints the verdict" grep -q "clean (round 2" "$T/v"
(cd "$W" && bash "$VERIFY" "$R1" >/dev/null 2>&1); rc=$?
check "verify.sh judges the exact commit, not the branch (R1 still 1)" test "$rc" -eq 1
review >/dev/null 2>&1; rc=$?
check "an already-clean HEAD reports clean without a call (exit 0)" test "$rc" -eq 0 -a "$(calls)" -eq 2

echo "=== verify.sh: missing and malformed notes ==="
commit_file x.txt "x" "unreviewed"
(cd "$W" && bash "$VERIFY" HEAD >/dev/null 2>&1); rc=$?
check "no note: exit 2 (got $rc)" test "$rc" -eq 2
g notes --ref=refs/notes/concrete-review add -f -m "verdict: clean" HEAD
(cd "$W" && bash "$VERIFY" HEAD >/dev/null 2>&1); rc=$?
check "malformed note (verdict line alone): exit 2 (got $rc)" test "$rc" -eq 2
B40="$(printf '%040d' 0 | tr 0 a)"
NO_REPO_NOTE="$(printf 'verdict: clean\nrounds: 2\nreviewer: codex\nwriter: claude\nbase: %s\npieces: 1\nutc: 2026-10-22T00:00:00Z' "$B40")"
g notes --ref=refs/notes/concrete-review add -f -m "$NO_REPO_NOTE" HEAD
out="$(cd "$W" && bash "$VERIFY" HEAD 2>&1)"; rc=$?
check "a clean note without repository: readable is not clean (exit 1, got $rc)" test "$rc" -eq 1
check "the missing-repository refusal names the missing key" grep -q "missing repository: readable" <<<"$out"
# A note without writer is malformed (exit 2), never grandfathered (owner item 5, 2026-11-08).
NO_WRITER_NOTE="$(printf 'verdict: clean\nrounds: 2\nreviewer: codex\nbase: %s\npieces: 1\nutc: 2026-10-22T00:00:00Z\nrepository: readable' "$B40")"
g notes --ref=refs/notes/concrete-review add -f -m "$NO_WRITER_NOTE" HEAD
out="$(cd "$W" && bash "$VERIFY" HEAD 2>&1)"; rc=$?
check "a note without writer is malformed (exit 2, got $rc)" test "$rc" -eq 2
check "the missing-writer refusal says malformed review note (missing writer)" grep -q "malformed review note (missing writer)" <<<"$out"
f_no_writer="$(cd "$W" && bash "$VERIFY" --fields HEAD 2>&1)"; rc=$?
check "verify.sh --fields also refuses a note without writer (exit 2, got $rc)" test "$rc" -eq 2
# A current note notes.sh put wrote (keys in its own order, writer and repository after utc) verifies
# when its base is an ancestor of HEAD, and is refused when its base is not an ancestor of HEAD.
VALID_BASE="$(g rev-parse main)"
g notes --ref=refs/notes/concrete-review add -f -m "$(printf 'verdict: clean\nrounds: 2\nreviewer: codex\nbase: %s\npieces: 1\nutc: 2026-10-22T00:00:00Z\nwriter: claude\nrepository: readable' "$VALID_BASE")" HEAD
(cd "$W" && bash "$VERIFY" HEAD >"$T/vc" 2>&1); rc=$?
check "a readable independent clean note with utc before writer verifies (exit 0, got $rc)" test "$rc" -eq 0
g notes --ref=refs/notes/concrete-review add -f -m "$NO_REPO_NOTE
repository: readable" HEAD
out="$(cd "$W" && bash "$VERIFY" HEAD 2>&1)"; rc=$?
check "a clean note whose base is not an ancestor of HEAD is refused without --required-base (exit 1, got $rc)" test "$rc" -eq 1
check "non-ancestor base refusal names base and commit" grep -q "base $B40 is not an ancestor-or-equal of" <<<"$out"
g notes --ref=refs/notes/concrete-review add -f -m "$NO_REPO_NOTE" HEAD
review >"$T/unverified-rerun" 2>&1; rc=$?
check "review.sh re-reviews an unverified clean HEAD (missing repository: readable) instead of short-circuiting (exit $rc)" \
  test "$rc" -eq 0 -a "$(field repository)" = readable -a "$(field writer)" = claude -a "$(field rounds)" = 3
# The landing bot's keys went with it: a verdict carried across a rebase, or a landing order, is no
# longer a key a review note may hold.
for extra in "carried-from: $B40" "order: 1"; do
  g notes --ref=refs/notes/concrete-review add -f -m "$(printf 'verdict: clean\nrounds: 2\nreviewer: codex\nwriter: claude\nbase: %s\npieces: 1\nutc: 2026-10-22T00:00:00Z\n%s' "$B40" "$extra")" HEAD
  out="$(cd "$W" && bash "$VERIFY" HEAD 2>&1)"; rc=$?
  check "a note with '${extra%%:*}' is malformed: exit 2, bad ${extra%%:*} (got $rc)" test "$rc" -eq 2 -a -n "$(printf '%s' "$out" | grep -F "malformed review note (bad ${extra%%:*})")"
done
g notes --ref=refs/notes/concrete-review add -f -m "$(printf 'verdict: clean\nrounds: 2\nreviewer: codex\nwriter: claude\nutc: 2026-10-22T00:00:00Z\nbase: %s\npieces: 1\nsurprise: 1' "$B40")" HEAD
(cd "$W" && bash "$VERIFY" HEAD >/dev/null 2>&1); rc=$?
check "an unknown key is malformed: exit 2 (got $rc)" test "$rc" -eq 2
g notes --ref=refs/notes/concrete-review add -f -m "$(printf 'verdict: findings\nverdict: clean\nrounds: 2\nreviewer: codex\nwriter: claude\nutc: 2026-10-22T00:00:00Z\nbase: %s\npieces: 1' "$B40")" HEAD
(cd "$W" && bash "$VERIFY" HEAD >/dev/null 2>&1); rc=$?
check "a repeated key is malformed: exit 2 (got $rc)" test "$rc" -eq 2
g notes --ref=refs/notes/concrete-review remove HEAD 2>/dev/null
f_out="$(cd "$W" && bash "$VERIFY" --fields HEAD 2>&1)"; rc=$?
check "verify.sh --fields exits 2 when no review note is present (got $rc)" test "$rc" -eq 2
g notes --ref=refs/notes/concrete-review add -f -m "$NO_REPO_NOTE" HEAD
f_out="$(cd "$W" && bash "$VERIFY" --fields HEAD 2>&1)"; rc=$?
check "verify.sh --fields exits 0 on a 7-key clean note missing repository: readable even though clean provenance fails (got $rc)" test "$rc" -eq 0
check "verify.sh --fields emits validated rounds: 2 and writer: claude on the 7-key note" \
  bash -c 'grep -q "^verdict: clean$" <<<"$1" && grep -q "^rounds: 2$" <<<"$1" && grep -q "^writer: claude$" <<<"$1" && grep -q "^repository: $" <<<"$1"' _ "$f_out"
g notes --ref=refs/notes/concrete-review add -f -m "$(printf 'verdict: clean\nrounds: 5\nreviewer: codex\nwriter: codex\nutc: 2026-11-08T00:00:00Z\nbase: %s\npieces: 1\nrepository: readable' "$VALID_BASE")" HEAD
(cd "$W" && bash "$VERIFY" HEAD >/dev/null 2>&1); v_rc=$?
f_out="$(cd "$W" && bash "$VERIFY" --fields HEAD 2>&1)"; rc=$?
check "verify.sh --fields exits 0 on a provenance-invalid (self-reviewed) 8-key clean note at rounds: 5 while verify.sh exits 1 (v_rc=$v_rc, rc=$rc)" \
  test "$v_rc" -eq 1 -a "$rc" -eq 0 -a "$(sed -n 's/^rounds: //p' <<<"$f_out")" = 5
(cd "$W" && bash "$VERIFY" --fields --required-base "$VALID_BASE" HEAD >/dev/null 2>&1); rc=$?
check "verify.sh --fields refuses --required-base (exit 2, got $rc)" test "$rc" -eq 2
g notes --ref=refs/notes/concrete-review remove HEAD 2>/dev/null

echo "=== verify.sh and review.sh: required base coverage & delta base-widening prevention (A -> B -> C) ==="
COV_A="$(g rev-parse main)"
g checkout -q main && g checkout -qb coverage-sibling
commit_file sibling.txt "sibling commit S" "S"
COV_S="$(g rev-parse HEAD)"
g checkout -q main && g checkout -qb coverage
commit_file b_only.txt "unreviewed intermediate B" "B"
COV_B="$(g rev-parse HEAD)"
commit_file c_only.txt "reviewed head C" "C"
COV_C="$(g rev-parse HEAD)"
coverage_note() { # base: a current clean note on C whose review began at this base
  g notes --ref=refs/notes/concrete-review add -f -m "$(printf 'verdict: clean\nrounds: 1\nreviewer: codex\nwriter: claude\nutc: 2026-11-08T00:00:00Z\nbase: %s\npieces: 1\nrepository: readable' "$1")" "$COV_C"
}
coverage_note "$COV_S"
out="$(cd "$W" && bash "$VERIFY" "$COV_C" 2>&1)"; rc=$?
check "a note based at sibling commit S (not an ancestor of C) is refused by verify.sh (exit 1, got $rc)" test "$rc" -eq 1
check "sibling-base refusal names S and C" grep -q "base $COV_S is not an ancestor-or-equal of $COV_C" <<<"$out"
coverage_note "$COV_B"
(cd "$W" && bash "$VERIFY" "$COV_C" >/dev/null 2>&1); rc=$?
check "without a coverage requirement the clean note at B verifies (exit 0, got $rc)" test "$rc" -eq 0
out="$(cd "$W" && bash "$VERIFY" --required-base "$COV_A" "$COV_C" 2>&1)"; rc=$?
check "a note based at unreviewed B does not cover required base A (exit 1, got $rc)" test "$rc" -eq 1
check "the coverage refusal names both bases" bash -c "grep -q 'base $COV_B.*required base $COV_A' <<<'$out'"
coverage_note "$COV_A"
(cd "$W" && bash "$VERIFY" --required-base "$COV_A" "$COV_C" >/dev/null 2>&1); rc=$?
check "a note based at main A covers required base A (exit 0, got $rc)" test "$rc" -eq 0
g notes --ref=refs/notes/concrete-review remove "$COV_C" 2>/dev/null

# Delta base-widening reproducer (A -> B -> C): if B carries a prior note (findings or clean) whose
# base is B (so A..B was never reviewed), running review.sh on C with base A must NOT start the delta
# at B and stamp base: A over B..C alone; it must review the full A..C diff.
for b_verdict in findings clean; do
  reset_fake
  b_repo=""
  [ "$b_verdict" = clean ] && b_repo=$'\nrepository: readable'
  g notes --ref=refs/notes/concrete-review add -f -m "$(printf 'verdict: %s\nrounds: 1\nreviewer: codex\nwriter: claude\nutc: 2026-11-08T00:00:00Z\nbase: %s\npieces: 1%s' "$b_verdict" "$COV_B" "$b_repo")" "$COV_B"
  review --base "$COV_A" >"$T/widen-$b_verdict" 2>&1; rc=$?
  check "delta base-widening ($b_verdict on B with base B): review on C against A exits 0 (got $rc)" test "$rc" -eq 0
  check "delta base-widening ($b_verdict on B with base B): reviews full A..C (both b_only.txt and c_only.txt)" \
    grep -q "files=b_only.txt,c_only.txt,$" "$FAKE_LOG/calls"
  check "delta base-widening ($b_verdict on B with base B): prompt states complete change against A, not delta from B" \
    grep -q "complete change of a branch against its base commit $COV_A" "$FAKE_LOG/prompt.1"
  check "delta base-widening ($b_verdict on B with base B): round increments to 2 and note base is A" \
    test "$(field rounds "$COV_C")/$(field base "$COV_C")" = "2/$COV_A"
  g notes --ref=refs/notes/concrete-review remove "$COV_C" 2>/dev/null
done
g notes --ref=refs/notes/concrete-review remove "$COV_B" 2>/dev/null

echo "=== independent reviewers and writer provenance ==="
for reviewer in kimi agy; do
  reset_fake
  g checkout -q main && g checkout -qb "reviewer-$reviewer"
  commit_file "$reviewer.txt" "clean review by $reviewer" "Codex-written branch"
  stamp_writer codex 1 "reviewer-$reviewer"
  review --reviewer "$reviewer" >"$T/reviewer-$reviewer" 2>&1; rc=$?
  check "Codex-stamped branch reviewed clean by $reviewer without --writer flag (exit $rc)" test "$rc" -eq 0
  check "$reviewer note records reviewer $reviewer and derived writer codex" \
    test "$(field reviewer)/$(field writer)" = "$reviewer/codex"
  check "verify accepts the independent $reviewer note and cross-checks writer provenance" \
    bash -c "cd '$W' && bash '$VERIFY' HEAD >/dev/null 2>&1"
  check "$reviewer runs in the read-only archive with stdin closed" grep -q "$reviewer archive=yes stdin_bytes=0" "$FAKE_LOG/calls"
  check "$reviewer runs with optional locks off" grep -q " optional_locks=0$" "$FAKE_LOG/locks"
done
# --writer is an optional assertion checked against derived writer provenance.
reset_fake
g checkout -q main && g checkout -qb writer-assert
commit_file assert.txt "agy-written" "agy commit"
stamp_writer agy 1 writer-assert
REVIEW_TEST_WRITER=codex review --reviewer kimi >"$T/writer-mismatch" 2>&1; rc=$?
check "mismatched --writer assertion is refused (exit 2, got $rc)" test "$rc" -eq 2
check "mismatched --writer error names the derived writer" grep -q "does not match derived writer provenance 'agy'" "$T/writer-mismatch"
before="$(calls)"
review --reviewer agy >"$T/self-review" 2>&1; rc=$?
check "reviewer == derived writer is refused without --writer flag (exit 2, got $rc)" test "$rc" -eq 2
check "self-review refusal calls no reviewer" test "$(calls)" = "$before"
REVIEW_TEST_WRITER=agy review --reviewer kimi >"$T/writer-match" 2>&1; rc=$?
check "matching --writer assertion succeeds (exit $rc)" test "$rc" -eq 0
# Multi-writer branch across rounds (round 1 codex, round 2 agy): reviewer must be none of them.
reset_fake
g checkout -q main && g checkout -qb multi-writer
commit_file mw1.txt "round 1 by codex" "mw1"
stamp_writer codex 1 multi-writer
commit_file mw2.txt "round 2 by agy" "mw2"
stamp_writer agy 2 multi-writer
review --reviewer codex >"$T/mw-codex" 2>&1; rc=$?
check "multi-writer branch (codex,agy) refuses codex as reviewer (exit 2, got $rc)" test "$rc" -eq 2
review --reviewer agy >"$T/mw-agy" 2>&1; rc=$?
check "multi-writer branch (codex,agy) refuses agy as reviewer (exit 2, got $rc)" test "$rc" -eq 2
review --reviewer kimi >"$T/mw-kimi" 2>&1; rc=$?
check "multi-writer branch (codex,agy) accepts kimi as independent reviewer (exit 0, got $rc)" test "$rc" -eq 0
check "multi-writer note records writer: codex,agy" test "$(field writer)" = "codex,agy"
(cd "$W" && bash "$VERIFY" HEAD >/dev/null 2>&1); rc=$?
check "verify.sh accepts clean note on multi-writer branch (exit 0, got $rc)" test "$rc" -eq 0
LOG_BLOB="$(python3 -I "$ROOT_DIR/scripts/review/writer_provenance.py" export-log --repo "$W" --base "$(g merge-base HEAD main)" --head HEAD --write-blob)"
check "export-log --write-blob writes a git blob that verifies multi-writer provenance" \
  python3 -I "$ROOT_DIR/scripts/review/writer_provenance.py" verify --repo "$W" --base "$(g merge-base HEAD main)" --head HEAD --claimed-writer "codex,agy" --reviewer kimi --log-object "$LOG_BLOB"
# Unstamped commits are NEVER attributed to claude by default: an unstamped commit has an UNKNOWN
# writer and is refused unless the writer log holds an explicit backfilled entry for it.
reset_fake
g checkout -q main && g checkout -qb unstamped-refused
commit_unstamped unstamped.txt "unstamped commit" "unstamped commit"
UNSTAMPED_SHA="$(g rev-parse HEAD)"
UNSTAMPED_BASE="$(g merge-base HEAD main)"
python3 -I "$ROOT_DIR/scripts/review/writer_provenance.py" derive --repo "$W" --base "$UNSTAMPED_BASE" --head "$UNSTAMPED_SHA" >"$T/unstamped-derive" 2>&1; rc=$?
check "unstamped non-merge commit is refused by writer_provenance.py derive (exit 2, got $rc)" test "$rc" -eq 2
check "unstamped commit error says unknown writer, never attributed to claude by default" \
  grep -q "unknown writer; unstamped commits are never attributed to claude by default" "$T/unstamped-derive"
before="$(calls)"
review --reviewer codex >"$T/unstamped-review" 2>&1; rc=$?
check "review.sh refuses a branch with an unstamped commit (exit 2, got $rc) and calls no reviewer" \
  test "$rc" -eq 2 -a "$(calls)" = "$before"
g notes --ref=refs/notes/concrete-review add -f -m "$(printf 'verdict: clean\nrounds: 1\nreviewer: codex\nwriter: claude\nutc: 2026-11-08T00:00:00Z\nbase: %s\npieces: 1\nrepository: readable' "$UNSTAMPED_BASE")" "$UNSTAMPED_SHA"
out="$(cd "$W" && bash "$VERIFY" "$UNSTAMPED_SHA" 2>&1)"; rc=$?
check "verify.sh refuses a clean note claiming writer: claude on an unstamped commit (exit 1, got $rc)" test "$rc" -eq 1
check "verify.sh unstamped refusal names the unknown writer" grep -q "unknown writer" <<<"$out"
g notes --ref=refs/notes/concrete-review remove "$UNSTAMPED_SHA" 2>/dev/null

# Backfill subcommand and wip/review-access scenario: rounds 1 & 2 unstamped by kimi, rounds 3 & 4
# harness-stamped by agy. Before backfilling, provenance is refused; after backfilling rounds 1 & 2
# as kimi, kimi is refused as reviewer of its own work, agy is refused, codex is accepted, and the
# reader distinguishes and reports both harness and backfilled entries.
reset_fake
g checkout -q main && g checkout -qb backfill-kimi-agy
commit_unstamped r1.txt "round 1 by kimi (unstamped)" "r1 kimi"
BF_R1="$(g rev-parse HEAD)"
commit_unstamped r2.txt "round 2 by kimi (unstamped)" "r2 kimi"
BF_R2="$(g rev-parse HEAD)"
commit_file r3.txt "round 3 by agy (stamped)" "r3 agy"
stamp_writer agy 3 backfill-kimi-agy
BF_R3="$(g rev-parse HEAD)"
commit_file r4.txt "round 4 by agy (stamped)" "r4 agy"
stamp_writer agy 4 backfill-kimi-agy
BF_R4="$(g rev-parse HEAD)"
BF_BASE="$(g merge-base HEAD main)"
review --reviewer kimi >"$T/bf-before" 2>&1; rc=$?
check "before backfilling rounds 1 & 2, review.sh refuses the branch (exit 2, got $rc)" test "$rc" -eq 2
# Backfill validation controls: cannot backfill a harness-stamped commit, or omit evidence / brief-file / output-file.
printf 'dummy brief\n' >"$T/ev/dummy.brief"
printf 'dummy output\n' >"$T/ev/dummy.out"
python3 -I "$ROOT_DIR/scripts/review/writer_provenance.py" backfill --repo "$W" \
  --commit "$BF_R3" --model agy --round 3 --branch backfill-kimi-agy \
  --brief-file "$T/ev/dummy.brief" --output-file "$T/ev/dummy.out" \
  --start-sha "$BF_R2" --end-sha "$BF_R3" --evidence "should fail" >"$T/bf-stamped-err" 2>&1; rc=$?
check "backfill refuses a commit that already carries a harness stamp (exit 2, got $rc)" test "$rc" -eq 2
python3 -I "$ROOT_DIR/scripts/review/writer_provenance.py" backfill --repo "$W" \
  --commit "$BF_R1" --model kimi --round 1 --branch backfill-kimi-agy \
  --brief-file "$T/ev/dummy.brief" --output-file "$T/ev/dummy.out" \
  --start-sha "$BF_BASE" --end-sha "$BF_R1" --evidence "   " >"$T/bf-empty-ev" 2>&1; rc=$?
check "backfill refuses whitespace-only evidence (exit 2, got $rc)" test "$rc" -eq 2
python3 -I "$ROOT_DIR/scripts/review/writer_provenance.py" backfill --repo "$W" \
  --commit "$BF_R1" --model kimi --round 1 --branch backfill-kimi-agy \
  --brief-file "$T/ev/dummy.brief" --output-file "$T/ev/dummy.out" \
  --brief-sha256 "3333333333333333333333333333333333333333333333333333333333333333" \
  --start-sha "$BF_BASE" --end-sha "$BF_R1" --evidence "digest mismatch" >"$T/bf-bad-digest" 2>&1; rc=$?
check "backfill refuses when --brief-sha256 disagrees with --brief-file sha256 (exit 2, got $rc)" test "$rc" -eq 2
backfill_writer "$BF_R1" kimi 1 backfill-kimi-agy "$BF_BASE" "$BF_R1" "kimi session log for round 1"
backfill_writer "$BF_R2" kimi 2 backfill-kimi-agy "$BF_R1" "$BF_R2" "kimi session log for round 2"
bf_note_r1="$(g notes --ref=refs/notes/concrete-writer show "$BF_R1")"
check "backfill writes source: backfill, entry-id, signer, SSH signature, output-sha256 and evidence to refs/notes/concrete-writer" \
  bash -c 'grep -qE "^source: backfill$" <<<"$1" && grep -qE "^entry-id: [0-9a-f]{64}$" <<<"$1" && grep -qE "^signer: orchestrator@concrete$" <<<"$1" && grep -q "^-----BEGIN SSH SIGNATURE-----$" <<<"$1" && grep -qE "^output-sha256: [0-9a-f]{64}$" <<<"$1" && grep -qE "^evidence: kimi session log for round 1$" <<<"$1"' _ "$bf_note_r1"
# Append-only log: backfill refuses to overwrite an existing entry without --supersedes and --reason.
python3 -I "$ROOT_DIR/scripts/review/writer_provenance.py" backfill --repo "$W" \
  --commit "$BF_R1" --model claude --round 1 --branch backfill-kimi-agy \
  --brief-file "$T/ev/dummy.brief" --output-file "$T/ev/dummy.out" \
  --start-sha "$BF_BASE" --end-sha "$BF_R1" --evidence "silent overwrite attempt" >"$T/bf-overwrite" 2>&1; rc=$?
check "backfill refuses to overwrite an existing writer-log entry (append-only, exit 2, got $rc)" test "$rc" -eq 2
check "backfill overwrite refusal explains the log is append-only and requires --supersedes" \
  grep -q "writer log is append-only and cannot be overwritten .* --supersedes" "$T/bf-overwrite"
review --reviewer kimi >"$T/bf-kimi-self" 2>&1; rc=$?
check "after backfilling rounds 1 & 2 as kimi, kimi is refused as reviewer of its own work (exit 2, got $rc)" test "$rc" -eq 2
check "kimi self-review refusal names derived writer kimi,agy" grep -q "reviewer kimi cannot review its own work (writer kimi,agy)" "$T/bf-kimi-self"
review --reviewer agy >"$T/bf-agy-self" 2>&1; rc=$?
check "agy is also refused as reviewer on the kimi,agy branch (exit 2, got $rc)" test "$rc" -eq 2
review --reviewer codex >"$T/bf-codex-ok" 2>&1; rc=$?
check "codex is accepted as independent reviewer on the kimi,agy branch (exit 0, got $rc)" test "$rc" -eq 0
check "review note records writer: kimi,agy" test "$(field writer)" = "kimi,agy"
rep_out="$(python3 -I "$ROOT_DIR/scripts/review/writer_provenance.py" report --repo "$W" --base "$BF_BASE" --head "$BF_R4")"
check "writer_provenance.py report distinguishes and reports both harness (2) and backfill (2) entries and 'evidence not re-checked'" \
  bash -c "grep -q 'writers: kimi,agy; harness (2): ${BF_R3:0:10}:agy@r3,${BF_R4:0:10}:agy@r4; backfill (2): ${BF_R1:0:10}:kimi@r1,${BF_R2:0:10}:kimi@r2; backfill-evidence: evidence not re-checked' <<<'$rep_out'"
rep_ev="$(python3 -I "$ROOT_DIR/scripts/review/writer_provenance.py" report --repo "$W" --base "$BF_BASE" --head "$BF_R4" --evidence-dir "$T/ev")"
check "writer_provenance.py report with --evidence-dir verifies brief and output sha256 against operator files" \
  grep -q "backfill-evidence: verified$" <<<"$rep_ev"
v_out="$(cd "$W" && bash "$VERIFY" "$BF_R4" 2>&1)"; rc=$?
check "verify.sh accepts clean note and reports both harness and backfilled provenance and 'evidence not re-checked' (exit 0, got $rc)" \
  bash -c "[ $rc -eq 0 ] && grep -q 'harness (2): ${BF_R3:0:10}:agy@r3,${BF_R4:0:10}:agy@r4; backfill (2): ${BF_R1:0:10}:kimi@r1,${BF_R2:0:10}:kimi@r2; backfill-evidence: evidence not re-checked' <<<'$v_out'"
v_ev="$(cd "$W" && bash "$VERIFY" --evidence-dir "$T/ev" "$BF_R4" 2>&1)"; rc=$?
check "verify.sh --evidence-dir re-checks brief and output files and reports backfill-evidence: verified (exit 0, got $rc)" \
  bash -c "[ $rc -eq 0 ] && grep -q 'backfill-evidence: verified' <<<'$v_ev'"
printf 'tampered brief\n' >"$T/ev-bad-brief"
v_bad_ev="$(cd "$W" && bash "$VERIFY" --brief-file "${BF_R1}=$T/ev-bad-brief" "$BF_R4" 2>&1)"; rc=$?
check "verify.sh --brief-file refuses when operator-supplied brief file does not match recorded brief-sha256 (exit 1, got $rc)" \
  bash -c "[ $rc -eq 1 ] && grep -q 'brief-sha256 mismatch' <<<'$v_bad_ev'"
BF_BLOB="$(python3 -I "$ROOT_DIR/scripts/review/writer_provenance.py" export-log --repo "$W" --base "$BF_BASE" --head "$BF_R4" --write-blob)"
check "export-log --write-blob preserves both backfilled and harness stanzas and verifies" \
  python3 -I "$ROOT_DIR/scripts/review/writer_provenance.py" verify --repo "$W" --base "$BF_BASE" --head "$BF_R4" --claimed-writer "kimi,agy" --reviewer codex --log-object "$BF_BLOB"

# Reviewer-conflict on backfill: backfill refuses attributing a commit to the note's reviewer unless
# an earlier signed entry already did so.
BF_R1_EID="$(sed -n 's/^entry-id: //p' <<<"$bf_note_r1")"
g notes --ref=refs/notes/concrete-review add -f -m "$(printf 'verdict: clean\nrounds: 1\nreviewer: codex\nwriter: kimi\nutc: 2026-11-08T00:00:00Z\nbase: %s\npieces: 1\nrepository: readable' "$BF_BASE")" "$BF_R1"
python3 -I "$ROOT_DIR/scripts/review/writer_provenance.py" backfill --repo "$W" \
  --commit "$BF_R1" --model codex --round 1 --branch backfill-kimi-agy \
  --brief-file "$T/ev/dummy.brief" --output-file "$T/ev/dummy.out" \
  --start-sha "$BF_BASE" --end-sha "$BF_R1" --evidence "attempt to attribute to note reviewer codex" \
  --supersedes "$BF_R1_EID" --reason "relabel to codex" >"$T/bf-rev-conflict" 2>&1; rc=$?
check "backfill refuses to attribute a commit to its review note's reviewer when no earlier signed entry did so (exit 2, got $rc)" test "$rc" -eq 2
check "backfill reviewer-conflict error names the reviewer rule" \
  grep -q "cannot attribute commit to 'codex', which is the reviewer" "$T/bf-rev-conflict"
g notes --ref=refs/notes/concrete-review remove "$BF_R1" 2>/dev/null

# Append-only correction & relabelling defence:
# Even if an unstamped Kimi commit is later superseded by a signed correction relabelling it to claude,
# the original Kimi stanza remains in the append-only log, the reader reports the full history, and
# Kimi remains disqualified from reviewing the commit (preventing relabelling an unstamped Kimi commit
# so Kimi can review its own work).
reset_fake
g checkout -q main && g checkout -qb append-only-correction
commit_unstamped corr.txt "unstamped kimi commit" "kimi work"
CORR_SHA="$(g rev-parse HEAD)"
CORR_BASE="$(g merge-base HEAD main)"
backfill_writer "$CORR_SHA" kimi 1 append-only-correction "$CORR_BASE" "$CORR_SHA" "initial kimi backfill"
CORR_EID1="$(g notes --ref=refs/notes/concrete-writer show "$CORR_SHA" | sed -n 's/^entry-id: //p')"
python3 -I "$ROOT_DIR/scripts/review/writer_provenance.py" backfill --repo "$W" \
  --commit "$CORR_SHA" --model claude --round 1 --branch append-only-correction \
  --brief-file "$T/ev/dummy.brief" --output-file "$T/ev/dummy.out" \
  --start-sha "$CORR_BASE" --end-sha "$CORR_SHA" --evidence "correction without reason" \
  --supersedes "$CORR_EID1" >"$T/corr-no-reason" 2>&1; rc=$?
check "backfill --supersedes without --reason is refused (exit 2, got $rc)" test "$rc" -eq 2
python3 -I "$ROOT_DIR/scripts/review/writer_provenance.py" backfill --repo "$W" \
  --commit "$CORR_SHA" --model claude --round 1 --branch append-only-correction \
  --brief-file "$T/ev/dummy.brief" --output-file "$T/ev/dummy.out" \
  --start-sha "$CORR_BASE" --end-sha "$CORR_SHA" --evidence "correction with wrong supersedes" \
  --supersedes "9999999999999999999999999999999999999999999999999999999999999999" --reason "wrong id" >"$T/corr-bad-id" 2>&1; rc=$?
check "backfill --supersedes with non-latest entry-id is refused (exit 2, got $rc)" test "$rc" -eq 2
backfill_writer "$CORR_SHA" claude 1 append-only-correction "$CORR_BASE" "$CORR_SHA" "corrected to claude" \
  "$CORR_EID1" "operator audit correction"
corr_note="$(g notes --ref=refs/notes/concrete-writer show "$CORR_SHA")"
check "append-only correction preserves the first stanza and appends the second stanza with supersedes and reason" \
  bash -c 'test "$(grep -c "^entry-id: " <<<"$1")" -eq 2 && grep -q "^supersedes: $2$" <<<"$1" && grep -q "^reason: operator audit correction$" <<<"$1"' _ "$corr_note" "$CORR_EID1"
corr_rep="$(python3 -I "$ROOT_DIR/scripts/review/writer_provenance.py" report --repo "$W" --base "$CORR_BASE" --head "$CORR_SHA")"
check "report uses latest valid entry (writers: claude) and reports full superseded history (kimi@r1 -> claude@r1)" \
  bash -c "grep -q '^writers: claude;' <<<'$corr_rep' && grep -q 'history: ${CORR_SHA:0:10}:kimi@r1\\[${CORR_EID1:0:10}\\]->claude@r1\\[' <<<'$corr_rep'"
review --reviewer kimi >"$T/corr-kimi-refused" 2>&1; rc=$?
check "relabelled commit still refuses kimi as reviewer because an earlier signed entry attributed it to kimi (exit 2, got $rc)" test "$rc" -eq 2
check "relabelled commit refusal names the earlier signed entry attribution" \
  grep -q "was attributed to kimi in earlier signed entry ${CORR_EID1:0:10}" "$T/corr-kimi-refused"
g notes --ref=refs/notes/concrete-review add -f -m "$(printf 'verdict: clean\nrounds: 1\nreviewer: kimi\nwriter: claude\nutc: 2026-11-08T00:00:00Z\nbase: %s\npieces: 1\nrepository: readable' "$CORR_BASE")" "$CORR_SHA"
v_corr_kimi="$(cd "$W" && bash "$VERIFY" "$CORR_SHA" 2>&1)"; rc=$?
check "verify.sh also refuses a clean note by kimi when an earlier superseded entry attributed the commit to kimi (exit 1, got $rc)" \
  bash -c "[ $rc -eq 1 ] && grep -q 'attributed to kimi in earlier signed entry' <<<'$v_corr_kimi'"
g notes --ref=refs/notes/concrete-review remove "$CORR_SHA" 2>/dev/null
review --reviewer codex >"$T/corr-codex-ok" 2>&1; rc=$?
check "codex is accepted as reviewer on the corrected commit and records writer: claude (exit 0, got $rc)" \
  test "$rc" -eq 0 -a "$(field writer "$CORR_SHA")" = "claude"

# Multi-commit note & exported-log replay controls (one authoritative history per commit):
# Reproducing a signed claude -> kimi correction on an ancestor commit, then appending its older
# signed claude stanza to the head's note (or supplying a truncated/conflicting --log-object) must
# be refused by validate-notes, derive, verify, and verify.sh.
reset_fake
g checkout -q main && g checkout -qb replay-ancestor-corr
commit_unstamped anc.txt "ancestor commit" "ancestor work"
REPLAY_ANC="$(g rev-parse HEAD)"
REPLAY_BASE="$(g merge-base HEAD main)"
backfill_writer "$REPLAY_ANC" claude 1 replay-ancestor-corr "$REPLAY_BASE" "$REPLAY_ANC" "initial claude attribution"
REPLAY_ANC_OLD_STANZA="$(g notes --ref=refs/notes/concrete-writer show "$REPLAY_ANC")"
REPLAY_ANC_EID1="$(sed -n 's/^entry-id: //p' <<<"$REPLAY_ANC_OLD_STANZA")"
commit_file head.txt "head commit by claude" "head work"
REPLAY_HEAD="$(g rev-parse HEAD)"
REPLAY_HEAD_STANZA="$(g notes --ref=refs/notes/concrete-writer show "$REPLAY_HEAD")"
# Export pre-correction log blob (before claude -> kimi correction on REPLAY_ANC):
REPLAY_TRUNC_BLOB="$(python3 -I "$ROOT_DIR/scripts/review/writer_provenance.py" export-log --repo "$W" --base "$REPLAY_BASE" --head "$REPLAY_HEAD" --write-blob)"
# Now append signed claude -> kimi correction on REPLAY_ANC:
backfill_writer "$REPLAY_ANC" kimi 1 replay-ancestor-corr "$REPLAY_BASE" "$REPLAY_ANC" "corrected to kimi" \
  "$REPLAY_ANC_EID1" "audit found ancestor was written by kimi"
g notes --ref=refs/notes/concrete-review add -f -m "$(printf 'verdict: clean\nrounds: 1\nreviewer: kimi\nwriter: claude\nutc: 2026-11-08T00:00:00Z\nbase: %s\npieces: 1\nrepository: readable' "$REPLAY_BASE")" "$REPLAY_HEAD"
v_replay_before="$(cd "$W" && bash "$VERIFY" "$REPLAY_HEAD" 2>&1)"; rc=$?
check "before replay: verify.sh refuses kimi clean note after claude->kimi correction on ancestor (exit 1, got $rc)" test "$rc" -eq 1
# Replay 1: append ancestor's older signed claude stanza to head's note.
SAVED_REPLAY_WNOTES="$(g rev-parse refs/notes/concrete-writer)"
g notes --ref=refs/notes/concrete-writer add -f -m "$(printf '%s\n\n%s' "$REPLAY_HEAD_STANZA" "$REPLAY_ANC_OLD_STANZA")" "$REPLAY_HEAD" 2>/dev/null
REPLAY_NOTES_SHA="$(g rev-parse refs/notes/concrete-writer)"
val_replay="$(python3 -I "$ROOT_DIR/scripts/review/writer_provenance.py" validate-notes --repo "$W" --notes-rev "$REPLAY_NOTES_SHA" 2>&1)"; rc=$?
check "validate-notes refuses a head note that embeds an ancestor's writer-log stanza (exit 1, got $rc)" \
  bash -c '[ "$1" -eq 1 ] && grep -q "records commit $3, not $4" <<<"$2"' _ "$rc" "$val_replay" "$REPLAY_ANC" "$REPLAY_HEAD"
drv_replay="$(python3 -I "$ROOT_DIR/scripts/review/writer_provenance.py" derive --repo "$W" --base "$REPLAY_BASE" --head "$REPLAY_HEAD" --reviewer kimi 2>&1)"; rc=$?
check "derive refuses when head note embeds an ancestor's older signed stanza (exit 2, got $rc)" test "$rc" -eq 2
v_replay_after="$(cd "$W" && bash "$VERIFY" "$REPLAY_HEAD" 2>&1)"; rc=$?
check "verify.sh still refuses kimi clean note when ancestor's older signed claude stanza is appended to head's note (exit 1, got $rc)" \
  bash -c '[ "$1" -eq 1 ] && grep -q "records commit $3, not $4" <<<"$2"' _ "$rc" "$v_replay_after" "$REPLAY_ANC" "$REPLAY_HEAD"
g update-ref refs/notes/concrete-writer "$SAVED_REPLAY_WNOTES"
# Replay 2: pass the truncated pre-correction exported log via --log-object.
trunc_ver="$(python3 -I "$ROOT_DIR/scripts/review/writer_provenance.py" verify --repo "$W" --base "$REPLAY_BASE" --head "$REPLAY_HEAD" --claimed-writer "claude" --reviewer kimi --log-object "$REPLAY_TRUNC_BLOB" 2>&1)"; rc=$?
check "verify with --log-object refuses a truncated pre-correction exported log against authoritative per-commit history (exit 1, got $rc)" \
  bash -c '[ "$1" -eq 1 ] && grep -q "has truncated history for commit" <<<"$2"' _ "$rc" "$trunc_ver"
trunc_drv="$(python3 -I "$ROOT_DIR/scripts/review/writer_provenance.py" derive --repo "$W" --base "$REPLAY_BASE" --head "$REPLAY_HEAD" --log-object "$REPLAY_TRUNC_BLOB" 2>&1)"; rc=$?
check "derive with --log-object refuses a truncated pre-correction exported log (exit 2, got $rc)" \
  bash -c '[ "$1" -eq 2 ] && grep -q "has truncated history for commit" <<<"$2"' _ "$rc" "$trunc_drv"
# Replay 3: pass a conflicting exported log via --log-object (different signed entry for REPLAY_ANC).
sed 's/^evidence: .*/evidence: conflicting signed backfill entry/' <<<"$REPLAY_ANC_OLD_STANZA" >"$T/replay-conflict-raw"
sign_raw_stanza "$T/replay-conflict-raw" "$T/replay-conflict-stanza"
printf '%s\n\n%s\n' "$(cat "$T/replay-conflict-stanza")" "$REPLAY_HEAD_STANZA" >"$T/replay-conflict.log"
conf_ver="$(python3 -I "$ROOT_DIR/scripts/review/writer_provenance.py" verify --repo "$W" --base "$REPLAY_BASE" --head "$REPLAY_HEAD" --claimed-writer "claude" --reviewer codex --log-object "$T/replay-conflict.log" 2>&1)"; rc=$?
check "verify with --log-object refuses a conflicting exported log against authoritative per-commit history (exit 1, got $rc)" \
  bash -c '[ "$1" -eq 1 ] && grep -q "conflicts with authoritative writer history for commit" <<<"$2"' _ "$rc" "$conf_ver"
g notes --ref=refs/notes/concrete-review remove "$REPLAY_HEAD" 2>/dev/null

# Allowed signers controls (Task A / owner item 7, approved 2026-11-08):
# 1. Entry signed by a throwaway key listed under its principal verifies (tested across BF_BASE..BF_R4).
listed_drv="$(python3 -I "$ROOT_DIR/scripts/review/writer_provenance.py" derive --repo "$W" --base "$BF_BASE" --head "$BF_R4" 2>&1)"; rc=$?
check "allowed_signers control 1: entry signed by a key listed under its principal verifies (exit 0, got $rc: $listed_drv)" \
  test "$rc" -eq 0 -a "$listed_drv" = "kimi,agy"
# 2. Entry signed by an unlisted throwaway key is refused.
SAVED_CORR_WNOTES="$(g rev-parse refs/notes/concrete-writer)"
g notes --ref=refs/notes/concrete-writer show "$CORR_SHA" >"$T/corr-saved-note"
awk '/^entry-id:/{exit} {print}' "$T/corr-saved-note" >"$T/corr-first-stanza-raw"
sign_raw_stanza "$T/corr-first-stanza-raw" "$T/corr-unlisted-stanza" "$T/keys/unauth_ed25519" "orchestrator@concrete"
g notes --ref=refs/notes/concrete-writer add -f -F "$T/corr-unlisted-stanza" "$CORR_SHA" 2>/dev/null
unlisted_drv="$(python3 -I "$ROOT_DIR/scripts/review/writer_provenance.py" derive --repo "$W" --base "$CORR_BASE" --head "$CORR_SHA" 2>&1)"; rc=$?
check "allowed_signers control 2: entry signed by an unlisted key is refused (exit 2, got $rc)" \
  bash -c '[ "$1" -eq 2 ] && grep -q "signing key is not authorized" <<<"$2"' _ "$rc" "$unlisted_drv"
# 3. Entry whose signature is cryptographically valid (key is listed in allowed_signers under another
#    principal, or entry claims a principal that does not match the allowed-signers line) is refused.
ssh-keygen -q -t ed25519 -N "" -C "other@concrete" -f "$T/keys/other_ed25519"
printf 'orchestrator@concrete namespaces="git" %s\nother@concrete namespaces="git" %s\n' \
  "$(cat "$T/keys/id_ed25519.pub")" "$(cat "$T/keys/other_ed25519.pub")" >"$T/keys/two_allowed_signers"
# Sign with id_ed25519 (listed under orchestrator@concrete) while claiming signer: other@concrete:
sign_raw_stanza "$T/corr-first-stanza-raw" "$T/corr-wrong-principal" "$T/keys/id_ed25519" "other@concrete"
g notes --ref=refs/notes/concrete-writer add -f -F "$T/corr-wrong-principal" "$CORR_SHA" 2>/dev/null
wrong_prin_drv="$(REVIEW_WRITER_ALLOWED_SIGNERS="$T/keys/two_allowed_signers" python3 -I "$ROOT_DIR/scripts/review/writer_provenance.py" derive --repo "$W" --base "$CORR_BASE" --head "$CORR_SHA" 2>&1)"; rc=$?
check "allowed_signers control 3: entry with valid signature whose principal does not match its allowed-signers line is refused (exit 2, got $rc)" \
  bash -c '[ "$1" -eq 2 ] && grep -q "signer '\''other@concrete'\'' does not match authorized principal(s) \['\''orchestrator@concrete'\''\]" <<<"$2"' _ "$rc" "$wrong_prin_drv"
g update-ref refs/notes/concrete-writer "$SAVED_CORR_WNOTES"
# 4. Shipped scripts/review/allowed_writer_log_signers parses, has fingerprint
#    SHA256:vx5cqlK2RywhQLRwaOCplg2BeBCnEBiZnxF8wn/CSLU, and contains the exact principals docs/REVIEW.md lists.
SHIPPED_SIGNERS="$ROOT_DIR/scripts/review/allowed_writer_log_signers"
shipped_parse="$(python3 -I - "$SHIPPED_SIGNERS" "$ROOT_DIR/docs/REVIEW.md" <<'PY'
import re, subprocess, sys
signers_path, doc_path = sys.argv[1], sys.argv[2]
active = [ln.strip() for ln in open(signers_path, encoding="utf-8") if ln.strip() and not ln.lstrip().startswith("#")]
if not active:
    sys.exit("no active lines in allowed_writer_log_signers")
principals = []
fps = []
for ln in active:
    m = re.match(r'^([^\s]+)\s+namespaces="git"\s+(ssh-ed25519\s+[A-Za-z0-9+/=]+)$', ln)
    if not m:
        sys.exit(f"malformed allowed_signers line: {ln}")
    principals.append(m.group(1))
    proc = subprocess.run(["ssh-keygen", "-lf", "-"], input=m.group(2) + "\n", capture_output=True, text=True)
    if proc.returncode != 0:
        sys.exit(f"ssh-keygen -lf failed: {proc.stderr.strip()}")
    fps.append(proc.stdout.strip().split()[1])
doc_text = open(doc_path, encoding="utf-8").read()
doc_principals = re.findall(r"Authorized principal:\s*`([^`]+)`", doc_text)
if principals != ["orchestrator@concrete"] or doc_principals != principals:
    sys.exit(f"principals mismatch: file={principals} docs={doc_principals}")
if fps != ["SHA256:vx5cqlK2RywhQLRwaOCplg2BeBCnEBiZnxF8wn/CSLU"]:
    sys.exit(f"fingerprint mismatch: {fps}")
print("ok")
PY
2>&1)"; rc=$?
check "allowed_signers control 4: shipped scripts/review/allowed_writer_log_signers parses and contains exactly the principals docs/REVIEW.md lists (exit 0, got $rc: $shipped_parse)" \
  test "$rc" -eq 0 -a "$shipped_parse" = "ok"
shipped_unlisted="$(REVIEW_WRITER_ALLOWED_SIGNERS="$SHIPPED_SIGNERS" python3 -I "$ROOT_DIR/scripts/review/writer_provenance.py" derive --repo "$W" --base "$BF_BASE" --head "$BF_R4" 2>&1)"; rc=$?
check "allowed_signers control 5: throwaway test key is refused by shipped allowed_writer_log_signers (exit 2, got $rc)" \
  bash -c '[ "$1" -eq 2 ] && grep -q "signing key is not authorized" <<<"$2"' _ "$rc" "$shipped_unlisted"
# 5. An empty allowed-signers file still fails closed.
printf '# empty allowed signers for fail-closed control\n' >"$T/keys/empty_allowed_signers"
empty_drv="$(REVIEW_WRITER_ALLOWED_SIGNERS="$T/keys/empty_allowed_signers" python3 -I "$ROOT_DIR/scripts/review/writer_provenance.py" derive --repo "$W" --base "$BF_BASE" --head "$BF_R4" 2>&1)"; rc=$?
check "with an empty allowed_writer_log_signers file, derive fails closed (exit 2, got $rc)" \
  bash -c '[ "$1" -eq 2 ] && grep -q "no authorized signers configured" <<<"$2"' _ "$rc" "$empty_drv"
empty_ver="$(cd "$W" && REVIEW_WRITER_ALLOWED_SIGNERS="$T/keys/empty_allowed_signers" bash "$VERIFY" "$BF_R4" 2>&1)"; rc=$?
check "with an empty allowed_writer_log_signers file, verify.sh fails closed (exit 1, got $rc)" test "$rc" -eq 1
empty_bf="$(REVIEW_WRITER_ALLOWED_SIGNERS="$T/keys/empty_allowed_signers" python3 -I "$ROOT_DIR/scripts/review/writer_provenance.py" backfill --repo "$W" \
  --commit "$UNSTAMPED_SHA" --model claude --round 1 --branch unstamped-refused \
  --brief-file "$T/ev/dummy.brief" --output-file "$T/ev/dummy.out" \
  --start-sha "$UNSTAMPED_BASE" --end-sha "$UNSTAMPED_SHA" --evidence "should fail closed" 2>&1)"; rc=$?
check "with an empty allowed_writer_log_signers file, backfill fails closed (exit 2, got $rc)" test "$rc" -eq 2

# Explicit claude backfill + harness-stamped codex commit -> derived writer is claude,codex
reset_fake
g checkout -q main && g checkout -qb human-plus-codex
commit_unstamped human.txt "unstamped human commit" "human commit"
backfill_writer HEAD claude 1 human-plus-codex "$(g rev-parse HEAD~1)" "$(g rev-parse HEAD)" "interactive claude commit"
commit_file bot.txt "stamped codex commit" "bot commit"
stamp_writer codex 2 human-plus-codex
review --reviewer codex >"$T/hc-codex" 2>&1; rc=$?
check "backfilled-claude+codex branch derives claude,codex and refuses codex reviewer (exit 2, got $rc)" test "$rc" -eq 2
review --reviewer kimi >"$T/hc-kimi" 2>&1; rc=$?
check "backfilled-claude+codex branch reviewed clean by kimi records writer: claude,codex (exit $rc)" \
  test "$rc" -eq 0 -a "$(field writer)" = "claude,codex"

# Merge commit provenance:
# 1. A merge with NO resolution (tree == conflict-free git merge-tree --write-tree) adds no writer.
reset_fake
g checkout -q main && g checkout -qb merge-side-clean
commit_file side_clean.txt "side clean content" "side clean"
stamp_writer codex 1 merge-side-clean
g checkout -q main && g checkout -qb merge-main-clean
commit_file main_clean.txt "main clean content" "main clean"
stamp_writer codex 1 merge-main-clean
g merge -q --no-ff merge-side-clean -m "clean merge with no conflict resolution"
CLEAN_MERGE_BASE="$(g rev-parse main)"
CLEAN_MERGE_HEAD="$(g rev-parse HEAD)"
drv_clean="$(python3 -I "$ROOT_DIR/scripts/review/writer_provenance.py" derive --repo "$W" --base "$CLEAN_MERGE_BASE" --head "$CLEAN_MERGE_HEAD" 2>&1)"; rc=$?
check "an unstamped clean merge commit (no resolution) adds no writer and derives codex (exit 0, got $rc: $drv_clean)" \
  test "$rc" -eq 0 -a "$drv_clean" = "codex"
stamp_writer agy 2 merge-main-clean
drv_clean_stamped="$(python3 -I "$ROOT_DIR/scripts/review/writer_provenance.py" derive --repo "$W" --base "$CLEAN_MERGE_BASE" --head HEAD 2>&1)"; rc=$?
check "a clean merge commit (no resolution) adds no writer even if stamped (exit 0, got $rc: $drv_clean_stamped)" \
  test "$rc" -eq 0 -a "$drv_clean_stamped" = "codex"
# 2. A merge WITH a conflict resolution (differs from git's automatic merge) is authored work:
#    refused when unstamped, accepted and attributed when harness-stamped with Writer: agy + Agy-Diff-SHA256.
g checkout -q main && g checkout -qb merge-side-conflict
commit_file conflict.txt "side version of conflict.txt" "side conflict"
stamp_writer codex 1 merge-side-conflict
g checkout -q main && g checkout -qb merge-main-conflict
commit_file conflict.txt "main version of conflict.txt" "main conflict"
stamp_writer codex 1 merge-main-conflict
g merge -q --no-commit merge-side-conflict >/dev/null 2>&1 || true
printf 'resolved version of conflict.txt by agy\n' >"$W/conflict.txt"
g add conflict.txt && g commit -qm "resolved merge conflict"
UNSTAMPED_MERGE_HEAD="$(g rev-parse HEAD)"
python3 -I "$ROOT_DIR/scripts/review/writer_provenance.py" derive --repo "$W" --base "$CLEAN_MERGE_BASE" --head "$UNSTAMPED_MERGE_HEAD" >"$T/merge-unstamped" 2>&1; rc=$?
check "an unstamped merge commit with a conflict resolution is refused as unknown writer (exit 2, got $rc)" test "$rc" -eq 2
check "unstamped resolved-merge error names the merge resolution rule" \
  grep -q "has a merge resolution (differs from git's automatic merge) but has an unknown writer" "$T/merge-unstamped"
stamp_writer agy 2 merge-main-conflict
STAMPED_MERGE_HEAD="$(g rev-parse HEAD)"
drv_resolved="$(python3 -I "$ROOT_DIR/scripts/review/writer_provenance.py" derive --repo "$W" --base "$CLEAN_MERGE_BASE" --head "$STAMPED_MERGE_HEAD" 2>&1)"; rc=$?
check "a resolved merge stamped with Writer: agy and Agy-Diff-SHA256 derives codex,agy (exit 0, got $rc: $drv_resolved)" \
  test "$rc" -eq 0 -a "$drv_resolved" = "codex,agy"
review --reviewer agy >"$T/merge-agy-refused" 2>&1; rc=$?
check "agy cannot review a branch whose merge conflict resolution it authored (exit 2, got $rc)" test "$rc" -eq 2
review --reviewer kimi >"$T/merge-kimi-ok" 2>&1; rc=$?
check "kimi reviews the codex,agy resolved-merge branch clean (exit 0, got $rc)" test "$rc" -eq 0
# 3. Evil merge (parents merge cleanly in git merge-tree, but the merge commit alters the tree) is
#    also authored work and refused when unstamped.
g checkout -q main && g checkout -qb merge-evil
commit_file evil_main.txt "evil main" "evil main"
stamp_writer codex 1 merge-evil
g merge -q --no-ff --no-commit merge-side-clean >/dev/null 2>&1
printf 'smuggled change inside a conflict-free merge\n' >>"$W/evil_main.txt"
g add evil_main.txt && g commit -qm "evil merge"
python3 -I "$ROOT_DIR/scripts/review/writer_provenance.py" derive --repo "$W" --base "$CLEAN_MERGE_BASE" --head HEAD >"$T/merge-evil" 2>&1; rc=$?
check "an unstamped conflict-free merge whose tree differs from git merge-tree is refused (exit 2, got $rc)" test "$rc" -eq 2

# Negative controls for incomplete or forged writer stamps:
# 1. Writer: trailer alone without a matching writer-log entry in refs/notes/concrete-writer is refused.
reset_fake
g checkout -q main && g checkout -qb trailer-only
commit_unstamped t.txt "trailer only" "$(printf 'forged stamp\n\nWriter: codex')"
review --reviewer kimi >"$T/trailer-only" 2>&1; rc=$?
check "a commit with a Writer: trailer but no author stamp or log entry is refused (exit 2, got $rc)" test "$rc" -eq 2
g checkout -q main && g checkout -qb author-and-trailer-no-log
SAVED_NOLOG_WNOTES="$(g rev-parse refs/notes/concrete-writer)"
commit_unstamped t2.txt "no log" "no log"
stamp_writer codex 1 author-and-trailer-no-log
g update-ref refs/notes/concrete-writer "$SAVED_NOLOG_WNOTES"
review --reviewer kimi >"$T/no-log" 2>&1; rc=$?
check "a commit with writer author and Writer: trailer but no writer-log entry is refused by review.sh (exit 2, got $rc)" test "$rc" -eq 2
check "no-log refusal says a Writer: trailer alone is never enough" grep -q "Writer: trailer alone is never enough" "$T/no-log"
NO_LOG_BASE="$(g merge-base HEAD main)"
g notes --ref=refs/notes/concrete-review add -f -m "$(printf 'verdict: clean\nrounds: 1\nreviewer: kimi\nwriter: codex\nutc: 2026-11-08T00:00:00Z\nbase: %s\npieces: 1\nrepository: readable' "$NO_LOG_BASE")" HEAD 2>/dev/null
out="$(cd "$W" && bash "$VERIFY" HEAD 2>&1)"; rc=$?
check "verify.sh also refuses a clean note when the commit's writer-log entry is missing (exit 1, got $rc)" test "$rc" -eq 1
check "verify.sh no-log refusal names the missing writer-log entry" grep -q "no matching writer-log entry" <<<"$out"
# 2. agy commit with mismatched agy-diff-sha256 in writer-log (even if validly signed) is refused;
#    tampered stanza without re-signing, unsigned stanza, and stanza signed by an unauthorized key are also refused.
g checkout -q main && g checkout -qb agy-bad-hash
commit_unstamped abh.txt "agy" "agy"
stamp_writer agy 1 agy-bad-hash
ABH_SHA="$(g rev-parse HEAD)"
SAVED_ABH_WNOTES="$(g rev-parse refs/notes/concrete-writer)"
g notes --ref=refs/notes/concrete-writer show "$ABH_SHA" >"$T/good-agy-log"
sed 's/^agy-diff-sha256: .*/agy-diff-sha256: 9999999999999999999999999999999999999999999999999999999999999999/' "$T/good-agy-log" >"$T/tampered-agy-log"
g notes --ref=refs/notes/concrete-writer add -f -F "$T/tampered-agy-log" "$ABH_SHA" 2>/dev/null
review --reviewer kimi >"$T/agy-tampered-out" 2>&1; rc=$?
check "a writer-log stanza tampered after signing is refused by entry-id/signature verification (exit 2, got $rc)" \
  bash -c '[ "$1" -eq 2 ] && grep -q "entry-id mismatch" "$2"' _ "$rc" "$T/agy-tampered-out"
awk '/^entry-id:/{exit} {print}' "$T/good-agy-log" >"$T/unsigned-agy-log"
g notes --ref=refs/notes/concrete-writer add -f -F "$T/unsigned-agy-log" "$ABH_SHA" 2>/dev/null
review --reviewer kimi >"$T/agy-unsigned-out" 2>&1; rc=$?
check "an unsigned writer-log stanza is refused (exit 2, got $rc)" \
  bash -c '[ "$1" -eq 2 ] && grep -q "missing entry-id, signer, or SSH signature" "$2"' _ "$rc" "$T/agy-unsigned-out"
sign_raw_stanza "$T/good-agy-log" "$T/unauth-agy-log" "$T/keys/unauth_ed25519" "unauth@concrete-test"
g notes --ref=refs/notes/concrete-writer add -f -F "$T/unauth-agy-log" "$ABH_SHA" 2>/dev/null
review --reviewer kimi >"$T/agy-unauth-out" 2>&1; rc=$?
check "a writer-log stanza signed by an unauthorized SSH key is refused (exit 2, got $rc)" \
  bash -c '[ "$1" -eq 2 ] && grep -q "signing key is not authorized" "$2"' _ "$rc" "$T/agy-unauth-out"
sign_raw_stanza "$T/tampered-agy-log" "$T/bad-agy-log"
g update-ref refs/notes/concrete-writer "$SAVED_ABH_WNOTES"
g notes --ref=refs/notes/concrete-writer add -f -F "$T/bad-agy-log" "$ABH_SHA" 2>/dev/null
BAD_WRITER_NOTES_SHA="$(g rev-parse refs/notes/concrete-writer)"
review --reviewer kimi >"$T/agy-bad-out" 2>&1; rc=$?
check "agy commit whose writer-log agy-diff-sha256 mismatches its trailer is refused even when signed (exit 2, got $rc)" test "$rc" -eq 2
g update-ref refs/notes/concrete-writer "$SAVED_ABH_WNOTES"
# 3. Clean note whose writer: claims claude on a codex-stamped branch is refused by verify.sh.
g checkout -q reviewer-kimi
SELF_BASE="$(g merge-base HEAD main)"
g notes --ref=refs/notes/concrete-review add -f -m "$(printf 'verdict: clean\nrounds: 1\nreviewer: agy\nwriter: claude\nutc: 2026-11-08T00:00:00Z\nbase: %s\npieces: 1\nrepository: readable' "$SELF_BASE")" HEAD 2>/dev/null
out="$(cd "$W" && bash "$VERIFY" HEAD 2>&1)"; rc=$?
check "verify.sh refuses a clean note whose writer field disagrees with commit provenance (exit 1, got $rc)" test "$rc" -eq 1
check "provenance-mismatch refusal names both claimed and derived writers" grep -q "note writer 'claude' does not match derived writer provenance 'codex'" <<<"$out"
g notes --ref=refs/notes/concrete-review add -f -m "$(printf 'verdict: clean\nrounds: 1\nreviewer: agy\nwriter: agy\nutc: 2026-11-08T00:00:00Z\nbase: %s\npieces: 1\nrepository: readable' "$SELF_BASE")" HEAD
out="$(cd "$W" && bash "$VERIFY" HEAD 2>&1)"; rc=$?
check "verify refuses a clean note whose reviewer equals writer (exit 1, got $rc)" test "$rc" -eq 1
check "self-reviewed-note refusal names the rule" grep -q "agy reviewed its own work" <<<"$out"
g notes --ref=refs/notes/concrete-review add -f -m "$(printf 'verdict: clean\nrounds: 1\nreviewer: agy\nutc: 2026-11-08T00:00:00Z\nbase: %s\npieces: 1\nrepository: readable' "$SELF_BASE")" HEAD
out="$(cd "$W" && bash "$VERIFY" HEAD 2>&1)"; rc=$?
check "verify refuses a clean note without writer as malformed (exit 2, got $rc)" test "$rc" -eq 2
check "missing-writer note refusal says malformed review note (missing writer)" grep -q "malformed review note (missing writer)" <<<"$out"

echo "=== verdict and reviewer-output contract parsing ==="
parse_case() { # parse_case <marker> <want-exit> <want-verdict> <label> [reviewer]
  local marker="$1" want="$2" wantv="$3" label="$4" rev="${5:-codex}" rc tmo="${REVIEW_TIMEOUT:-1500}"
  [ "$marker" = MARK_DOUBLE_TIMEOUT ] && tmo=0.05
  reset_fake
  g checkout -q main && g checkout -qb "parse-$rev-$marker"
  commit_file "p/$marker.txt" "$(printf 'line %s\n' $(seq 1 20))
$marker" "case $marker"
  [ "$rev" = codex ] || stamp_writer codex 1 "parse-$rev-$marker"
  REVIEW_TIMEOUT="$tmo" review --reviewer "$rev" >/dev/null 2>&1; rc=$?
  check "$label (exit $rc, verdict $(field verdict))" test "$rc" = "$want" -a "$(field verdict)" = "$wantv"
}
parse_case MARK_BOLD 0 clean "markdown-emphasised READ/REPOSITORY/VERDICT lines are accepted"
parse_case MARK_TRAILING 5 blocked "text after the verdict line is not a verdict"
parse_case MARK_NOVERDICT 5 blocked "a reply with no verdict line is blocked, not clean"
parse_case MARK_ERROR 5 blocked "a reviewer that exits nonzero is blocked"
parse_case MARK_REPOSITORY_UNREADABLE 5 blocked "an unreadable repository is blocked, never clean"
parse_case MARK_SANDBOX_FAILURE 5 blocked "a sandbox failure text overrides VERDICT: clean"
parse_case MARK_NO_REPOSITORY 5 blocked "a clean verdict without a REPOSITORY line is blocked"
parse_case MARK_REPO_WRONG_POS 5 blocked "REPOSITORY: readable not on the penultimate non-empty line is blocked"
parse_case MARK_NO_READ 5 blocked "a clean verdict without any READ: <path> line is blocked"
parse_case MARK_BAD_READ 5 blocked "a READ: <path> naming a file not in the reviewed commit is blocked"
parse_case MARK_READ_DIR 5 blocked "a READ: <path> naming a directory instead of a file is blocked"
parse_case MARK_READ_ABS 5 blocked "a READ: <path> with an absolute path is blocked"
parse_case MARK_MUTATE_TREE 5 blocked "a reviewer that mutates the read-only archived tree is blocked"
parse_case MARK_MENTION_FILTER 0 clean "a real review that mentions content filters is not a refusal"
# Double timeout under bounded_run (exit 124) must block immediately without halving retry:
# bounded_run makes its 2 standard attempts, and review.sh makes 0 neutral halving-retry calls.
parse_case MARK_DOUBLE_TIMEOUT 5 blocked "a bounded_run double-timeout blocks immediately without halving retry"
check "double-timeout made only bounded_run's 2 standard attempts and 0 neutral halving retries" \
  bash -c "[ '$(calls)' -eq 2 ] && ! grep -q ' neutral ' '$FAKE_LOG/calls'"
# Kimi and agy negative controls across the shared reviewer-output contract.
for r in kimi agy; do
  parse_case MARK_REPOSITORY_UNREADABLE 5 blocked "$r: REPOSITORY: unreadable is blocked" "$r"
  parse_case MARK_NO_REPOSITORY 5 blocked "$r: missing REPOSITORY: readable is blocked" "$r"
  parse_case MARK_REPO_WRONG_POS 5 blocked "$r: non-penultimate REPOSITORY: readable is blocked" "$r"
  parse_case MARK_NO_READ 5 blocked "$r: missing READ: <path> is blocked" "$r"
  parse_case MARK_BAD_READ 5 blocked "$r: nonexistent READ: <path> is blocked" "$r"
  parse_case MARK_ERROR 5 blocked "$r: nonzero exit is blocked" "$r"
  parse_case MARK_DOUBLE_TIMEOUT 5 blocked "$r: double-timeout is blocked in 1 call" "$r"
done

echo "=== reviewer output collector: final message only, never stdout log (Task C) ==="
COLLECTOR="$ROOT_DIR/scripts/review/collect_output.sh"
mkdir -p "$T/col-tree"
printf 'base\n' >"$T/col-tree/README.md"
chmod -R a-w "$T/col-tree"
printf 'Review commit %s\n' "$SELF_BASE" >"$T/col-prompt.txt"
bash "$COLLECTOR" collect codex --tree "$T/col-tree" --prompt "$T/col-prompt.txt" --out "$T/col-out.txt" --log "$T/col-log.txt"; rc=$?
check "collect_output.sh collect codex succeeds (exit 0, got $rc)" test "$rc" -eq 0
check "codex stdout log carries the 'tokens used' trailer after VERDICT" grep -q "^tokens used$" "$T/col-log.txt"
check "codex -o final-message file omits the 'tokens used' trailer" bash -c "! grep -q 'tokens used' '$T/col-out.txt'"
log_parse="$(python3 -I "$ROOT_DIR/scripts/review/output_contract.py" --repo "$W" --commit "$SELF_BASE" --verdicts clean,findings <"$T/col-log.txt")"
check "a Codex stdout log carrying a 'tokens used' trailer after VERDICT is refused by output_contract.py" \
  bash -c 'grep -q "^status: noverdict$" <<<"$1" && grep -q "last non-empty line is not a valid VERDICT" <<<"$1"' _ "$log_parse"
out_parse="$(python3 -I "$ROOT_DIR/scripts/review/output_contract.py" --repo "$W" --commit "$SELF_BASE" --verdicts clean,findings <"$T/col-out.txt")"
check "the matching -o final-message file is accepted by output_contract.py" \
  grep -q "^status: clean$" <<<"$out_parse"
same_file_out="$(bash "$COLLECTOR" collect codex --tree "$T/col-tree" --prompt "$T/col-prompt.txt" --out "$T/col-same.txt" --log "$T/col-same.txt" 2>&1)"; rc=$?
check "collect_output.sh refuses when --out and --log point to the same file (exit 2, got $rc)" \
  bash -c '[ "$1" -eq 2 ] && grep -q "out-file and log-file must be distinct files" <<<"$2"' _ "$rc" "$same_file_out"
# Structural check on the shipped collector and callers (review.sh, signoff.sh), plus negative controls
# on broken collectors/callers that pass a log instead of the final-message file.
struct_ok="$(bash "$COLLECTOR" --check-structure 2>&1)"; rc=$?
check "structural check accepts shipped collect_output.sh, review.sh and signoff.sh (exit 0, got $rc)" test "$rc" -eq 0
sed 's/ -o "\$out_file"//;s/>"\$log_file" 2>&1/>"\$out_file" 2>"\$log_file"/' "$COLLECTOR" >"$T/bad-collector-stdout.sh"
bad_c1="$(bash "$COLLECTOR" --check-structure "$T/bad-collector-stdout.sh" 2>&1)"; rc=$?
check "structural check catches a collector that redirects codex exec stdout instead of using -o (exit 1, got $rc)" \
  bash -c '[ "$1" -eq 1 ] && grep -q "codex exec must write the final message via -o" <<<"$2"' _ "$rc" "$bad_c1"
sed 's/-o "\$out_file"/-o "\$log_file"/' "$COLLECTOR" >"$T/bad-collector-log.sh"
bad_c2="$(bash "$COLLECTOR" --check-structure "$T/bad-collector-log.sh" 2>&1)"; rc=$?
check "structural check catches a collector that writes -o to the log file (exit 1, got $rc)" \
  bash -c '[ "$1" -eq 1 ] && grep -q "codex exec must not write -o to the log file" <<<"$2"' _ "$rc" "$bad_c2"
sed 's/msg="\$(cat "\$out")"/msg="\$(cat "\$out")"; [ -n "\$msg" ] || msg="\$(cat "\$log")"/' "$REVIEW" >"$T/bad-caller-fallback.sh"
bad_c3="$(bash "$COLLECTOR" --check-structure "$COLLECTOR" "$T/bad-caller-fallback.sh" 2>&1)"; rc=$?
check "structural check catches a caller that falls back to reading \$log when \$out is empty (exit 1, got $rc)" \
  bash -c '[ "$1" -eq 1 ] && grep -q "falls back to or copies the diagnostic log" <<<"$2"' _ "$rc" "$bad_c3"
sed 's/collect_reviewer_output "\$REVIEWER" "\$REVIEW_TREE" "\$prompt" "\$out" "\$log"/collect_reviewer_output "\$REVIEWER" "\$REVIEW_TREE" "\$prompt" "\$log" "\$out"/' "$REVIEW" >"$T/bad-caller-swap.sh"
bad_c4="$(bash "$COLLECTOR" --check-structure "$COLLECTOR" "$T/bad-caller-swap.sh" 2>&1)"; rc=$?
check "structural check catches a caller that passes \$log in place of the final-message file (exit 1, got $rc)" \
  bash -c '[ "$1" -eq 1 ] && grep -q "passes log file in place of final-message out file" <<<"$2"' _ "$rc" "$bad_c4"
sed 's/validate_reviewer_output/python3 -I "\$OUTPUT_CONTRACT" --commit "\$sha" --verdicts ACCEPT,REJECT </g' "$ROOT_DIR/scripts/land/signoff.sh" >"$T/bad-signoff-novalidate.sh"
bad_c5="$(bash "$COLLECTOR" --check-structure "$COLLECTOR" "$T/bad-signoff-novalidate.sh" 2>&1)"; rc=$?
check "structural check catches a caller that bypasses validate_reviewer_output (exit 1, got $rc)" \
  bash -c '[ "$1" -eq 1 ] && grep -q "caller does not call validate_reviewer_output" <<<"$2"' _ "$rc" "$bad_c5"
sed 's/prepare_review_archive "\$repo_root" "\$sha" "\$tree" "\$manifest"/git archive "\$sha" | tar -x -C "\$tree"/' "$ROOT_DIR/scripts/land/signoff.sh" >"$T/bad-signoff-noarchive.sh"
bad_c6="$(bash "$COLLECTOR" --check-structure "$COLLECTOR" "$T/bad-signoff-noarchive.sh" 2>&1)"; rc=$?
check "structural check catches a caller that extracts git archive directly instead of using prepare_review_archive (exit 1, got $rc)" \
  bash -c '[ "$1" -eq 1 ] && grep -q "caller does not call prepare_review_archive" <<<"$2" && grep -q "invokes git archive directly" <<<"$2"' _ "$rc" "$bad_c6"
chmod -R u+w "$T/col-tree" 2>/dev/null || true

echo "=== content-filter refusal: one neutral retry on smaller pieces ==="
reset_fake
g checkout -q main && g checkout -qb refuse-once
commit_file Concrete/Own.lean "$(printf 'line %s\n' $(seq 1 40))
MARK_REFUSE_STANDARD" "ownership change"
review >"$T/out3" 2>&1; rc=$?
check "refused then reviewed neutrally: clean (exit $rc)" test "$rc" -eq 0
check "first call standard, retries neutral" \
  bash -c "awk '{print \$2}' '$FAKE_LOG/calls' | tr '\n' ' ' | has_match -E '^standard (neutral )+$'"
first_bytes="$(awk 'NR==1 { sub("bytes=", "", $4); print $4 }' "$FAKE_LOG/calls")"
retry_max="$(awk 'NR>1 { sub("bytes=", "", $4); if ($4 + 0 > m) m = $4 + 0 } END { print m + 0 }' "$FAKE_LOG/calls")"
check "retry pieces are smaller ($retry_max < $first_bytes)" test "$retry_max" -lt "$first_bytes"
check "neutral prompt avoids the project framing" bash -c "! grep -q 'Concrete, a compiler' '$FAKE_LOG/prompt.2'"
check "the neutral prompt carries the permanent owner rule" \
  grep -Fq 'OWNER RULE (2026-11-08, verbatim: "ALWAYS tell the reviewrs to do the best long term thing rather than cuitting corners"): judge and fix by the best long-term design. Never cut corners for speed, a smaller diff or fewer rounds. As a reviewer, report any shortcut, containment patch or corner cut as a real finding and say what the long-term fix is. As a writer, implement the structural fix, not a containment patch; if staging is truly unavoidable, record the long-term design and name its roadmap row.' "$FAKE_LOG/prompt.2"
check "raw output records the refusal" grep -q ": refused" "$W/.review/$(g rev-parse HEAD)-1.md"

reset_fake
g checkout -q main && g checkout -qb refuse-always
commit_file Concrete/Own.lean "$(printf 'line %s\n' $(seq 1 40))
MARK_REFUSE_ALWAYS" "ownership change"
review >"$T/out4" 2>&1; rc=$?
check "refused twice: blocked, exit 5 (got $rc)" test "$rc" -eq 5
check "note: verdict blocked" test "$(field verdict)" = blocked
check "no loop: one standard call and one retry pass ($(calls) calls)" test "$(calls)" -ge 2 -a "$(calls)" -le 3
check "blocked piece listed in the output" grep -q "blocked" "$T/out4"
reset_fake
review >/dev/null 2>&1; rc=$?
check "a blocked HEAD may be re-run; it counts as round 2" test "$(field rounds)" = 2 -a "$rc" -eq 5
check "the re-run reviews the whole branch again (blocked does not advance the delta)" \
  grep -q "complete change of a branch" "$FAKE_LOG/prompt.1"

echo "=== findings in one piece, another piece blocked ==="
reset_fake
g checkout -q main && g checkout -qb mixed
commit_file a.txt "MARK_FINDING here" "a finding"
commit_file Concrete/Own.lean "MARK_REFUSE_ALWAYS" "a refusal"
review >"$T/out8" 2>&1; rc=$?
check "blocked wins over findings: verdict blocked, exit 5 (got $rc, $(field verdict))" \
  test "$rc" -eq 5 -a "$(field verdict)" = blocked
check "the reviewed piece's findings are still recorded" \
  bash -c "git -C '$W' notes --ref=refs/notes/concrete-review-findings show HEAD | has_match 'counter is never reset'"
commit_file b.txt "more" "more work"
reset_fake
review >/dev/null 2>&1
check "next round restarts from the base, not after the blocked round" grep -q "complete change of a branch" "$FAKE_LOG/prompt.1"
check "and carries the blocked round's findings forward" grep -q "counter is never reset" "$FAKE_LOG/prompt.1"
# A blocked round that finds nothing new must still hand the old finding on.
commit_file a.txt "MARK_REFUSE_ALWAYS" "rewrite a.txt"
review >/dev/null 2>&1
check "a blocked round with no findings of its own records the carried ones" \
  bash -c "git -C '$W' notes --ref=refs/notes/concrete-review-findings show HEAD | has_match 'counter is never reset'"
commit_file c.txt "c" "c"
reset_fake
review >/dev/null 2>&1
check "after two blocked rounds in a row the finding still reaches the prompt" \
  grep -q "counter is never reset" "$FAKE_LOG/prompt.1"

echo "=== pre-push hook: a review-notes push runs no gates and semantically validates refs/notes/concrete-writer ==="
HOOK="$ROOT_DIR/.githooks/pre-push"
Z=0000000000000000000000000000000000000000
NOTES_SHA="$(g rev-parse refs/notes/concrete-review)"
WRITER_NOTES_SHA="$(g rev-parse refs/notes/concrete-writer)"
hook_out="$(cd "$W" && printf 'refs/notes/concrete-review %s refs/notes/concrete-review %s\nrefs/notes/concrete-review-findings %s refs/notes/concrete-review-findings %s\nrefs/notes/concrete-writer %s refs/notes/concrete-writer %s\n' \
  "$NOTES_SHA" "$Z" "$(g rev-parse refs/notes/concrete-review-findings)" "$Z" "$WRITER_NOTES_SHA" "$Z" | bash "$HOOK" 2>&1)"; rc=$?
check "notes-only push (review, findings, semantically valid writer notes): hook exits 0 (got $rc)" test "$rc" -eq 0
check "notes-only push: hook takes the review-notes tier" grep -q "review-notes push" <<<"$hook_out"
# Negative controls for pre-push hook semantic validation of refs/notes/concrete-writer:
wnote_hook() { (cd "$W" && printf 'refs/notes/concrete-writer %s refs/notes/concrete-writer %s\n' "$1" "${2:-$Z}" | bash "$HOOK" 2>&1); }
hook_out="$(wnote_hook "$BAD_WRITER_NOTES_SHA" "$Z")"; rc=$?
check "hook refuses refs/notes/concrete-writer with mismatched agy-diff-sha256 (exit 1, got $rc)" \
  bash -c '[ "$1" -eq 1 ] && grep -q "invalid refs/notes/concrete-writer" <<<"$2"' _ "$rc" "$hook_out"
g update-ref -d refs/notes/concrete-writer-bad 2>/dev/null || true
g notes --ref=refs/notes/concrete-writer-bad add -f -m "not a valid stanza" "$ABH_SHA" 2>/dev/null
hook_out="$(wnote_hook "$(g rev-parse refs/notes/concrete-writer-bad)" "$Z")"; rc=$?
check "hook refuses malformed stanza in refs/notes/concrete-writer (exit 1, got $rc)" \
  bash -c '[ "$1" -eq 1 ] && grep -q "malformed line" <<<"$2"' _ "$rc" "$hook_out"
g update-ref -d refs/notes/concrete-writer-bad 2>/dev/null || true
g notes --ref=refs/notes/concrete-writer-bad add -f -F "$T/good-agy-log" "$UNSTAMPED_SHA" 2>/dev/null
hook_out="$(wnote_hook "$(g rev-parse refs/notes/concrete-writer-bad)" "$Z")"; rc=$?
check "hook refuses a writer-log note whose commit field does not match the annotated commit (exit 1, got $rc)" \
  bash -c '[ "$1" -eq 1 ] && grep -q "records commit $3, not $4" <<<"$2"' _ "$rc" "$hook_out" "$ABH_SHA" "$UNSTAMPED_SHA"
sed "s/^commit: .*/commit: $UNSTAMPED_SHA/;s/^model: .*/model: codex/;/^agy-diff-sha256:/d" "$T/good-agy-log" >"$T/forged-harness-raw"
sign_raw_stanza "$T/forged-harness-raw" "$T/forged-harness-on-unstamped"
g update-ref -d refs/notes/concrete-writer-bad 2>/dev/null || true
g notes --ref=refs/notes/concrete-writer-bad add -f -F "$T/forged-harness-on-unstamped" "$UNSTAMPED_SHA" 2>/dev/null
hook_out="$(wnote_hook "$(g rev-parse refs/notes/concrete-writer-bad)" "$Z")"; rc=$?
check "hook refuses a harness writer-log note attached to an unstamped commit (exit 1, got $rc)" \
  bash -c '[ "$1" -eq 1 ] && grep -q "no '\''Writer: <model>'\'' trailer" <<<"$2"' _ "$rc" "$hook_out"
g notes --ref=refs/notes/concrete-writer show "$BF_R1" | sed "s/^commit: .*/commit: $ABH_SHA/;s|^start-sha: .*|start-sha: $(g rev-parse "$ABH_SHA~1")|;s/^end-sha: .*/end-sha: $ABH_SHA/" >"$T/backfill-on-stamped-raw"
sign_raw_stanza "$T/backfill-on-stamped-raw" "$T/backfill-on-stamped"
g update-ref -d refs/notes/concrete-writer-bad 2>/dev/null || true
g notes --ref=refs/notes/concrete-writer-bad add -f -F "$T/backfill-on-stamped" "$ABH_SHA" 2>/dev/null
hook_out="$(wnote_hook "$(g rev-parse refs/notes/concrete-writer-bad)" "$Z")"; rc=$?
check "hook refuses a backfill writer-log note attached to a harness-stamped commit (exit 1, got $rc)" \
  bash -c '[ "$1" -eq 1 ] && grep -q "carries a harness commit stamp and cannot use a backfilled writer-log entry" <<<"$2"' _ "$rc" "$hook_out"
g update-ref refs/notes/concrete-writer-del "$WRITER_NOTES_SHA"
g notes --ref=refs/notes/concrete-writer-del remove "$ABH_SHA" 2>/dev/null
hook_out="$(wnote_hook "$(g rev-parse refs/notes/concrete-writer-del)" "$WRITER_NOTES_SHA")"; rc=$?
check "hook refuses a refs/notes/concrete-writer update that deletes an existing writer-log entry (exit 1, got $rc)" \
  bash -c '[ "$1" -eq 1 ] && grep -q "deletes existing writer-log entry" <<<"$2"' _ "$rc" "$hook_out"
# Pre-push append-only check: replacing an existing backfilled entry on BF_R1 with a different validly signed entry
# (instead of appending a superseding entry) is refused by validate-notes when old_notes_rev is supplied.
g notes --ref=refs/notes/concrete-writer show "$BF_R1" | sed "s/^model: .*/model: claude/" >"$T/replaced-bf-r1-raw"
sign_raw_stanza "$T/replaced-bf-r1-raw" "$T/replaced-bf-r1"
g update-ref refs/notes/concrete-writer-replace "$WRITER_NOTES_SHA"
g notes --ref=refs/notes/concrete-writer-replace add -f -F "$T/replaced-bf-r1" "$BF_R1" 2>/dev/null
hook_out="$(wnote_hook "$(g rev-parse refs/notes/concrete-writer-replace)" "$WRITER_NOTES_SHA")"; rc=$?
check "hook refuses a refs/notes/concrete-writer update that rewrites/replaces an existing entry instead of appending (exit 1, got $rc)" \
  bash -c '[ "$1" -eq 1 ] && grep -q "rewrites or drops existing writer-log entry" <<<"$2"' _ "$rc" "$hook_out"
# Absent nonzero old_notes_rev vs all-zero creation sentinel:
# Supplying a nonzero old notes SHA that is not available locally must fail closed (never be treated as
# initial creation skipping deletion/replacement checks), whereas the all-zero creation sentinel Z is accepted.
ABSENT_OLD_NOTES_SHA="deadbeefdeadbeefdeadbeefdeadbeefdeadbeef"
hook_out="$(wnote_hook "$(g rev-parse refs/notes/concrete-writer-replace)" "$ABSENT_OLD_NOTES_SHA")"; rc=$?
check "hook refuses a replacement notes tree when supplied a nonzero old SHA not available locally (exit 1, got $rc)" \
  bash -c '[ "$1" -eq 1 ] && grep -q "old-notes-rev '\''deadbeefdeadbeefdeadbeefdeadbeefdeadbeef'\'' is not available locally" <<<"$2"' _ "$rc" "$hook_out"
hook_out="$(wnote_hook "$WRITER_NOTES_SHA" "$ABSENT_OLD_NOTES_SHA")"; rc=$?
check "hook refuses even a valid notes tree when supplied an absent nonzero old SHA, distinguishing it from the all-zero creation sentinel (exit 1, got $rc)" \
  bash -c '[ "$1" -eq 1 ] && grep -q "every nonzero old notes SHA must resolve and be compared" <<<"$2"' _ "$rc" "$hook_out"
hook_out="$(wnote_hook "$WRITER_NOTES_SHA" "$Z")"; rc=$?
check "hook accepts the valid notes tree when supplied the all-zero creation sentinel (exit 0, got $rc)" test "$rc" -eq 0
empty_old_out="$(python3 -I "$ROOT_DIR/scripts/review/writer_provenance.py" validate-notes --repo "$W" --notes-rev "$WRITER_NOTES_SHA" --old-notes-rev "" 2>&1)"; rc=$?
check "validate-notes refuses an empty --old-notes-rev argument (exit 1, got $rc)" \
  bash -c '[ "$1" -eq 1 ] && grep -q "old-notes-rev is empty" <<<"$2"' _ "$rc" "$empty_old_out"
# Appending a valid signed superseding entry on BF_R2 is accepted by the pre-push hook against WRITER_NOTES_SHA.
BF_R2_OLD_NOTE="$(g notes --ref=refs/notes/concrete-writer show "$BF_R2")"
BF_R2_EID="$(sed -n 's/^entry-id: //p' <<<"$BF_R2_OLD_NOTE")"
backfill_writer "$BF_R2" claude 2 backfill-kimi-agy "$BF_R1" "$BF_R2" "append correction on BF_R2" \
  "$BF_R2_EID" "audit test of append-only push"
APPENDED_WRITER_NOTES_SHA="$(g rev-parse refs/notes/concrete-writer)"
BF_R2_APPENDED_NOTE="$(g notes --ref=refs/notes/concrete-writer show "$BF_R2")"
hook_out="$(wnote_hook "$APPENDED_WRITER_NOTES_SHA" "$WRITER_NOTES_SHA")"; rc=$?
check "hook accepts a refs/notes/concrete-writer update that appends a signed superseding entry (exit 0, got $rc)" test "$rc" -eq 0
g update-ref refs/notes/concrete-writer "$WRITER_NOTES_SHA"
# Historical pre-contract writer note on an already-landed commit (main base) is NOT re-judged when
# unchanged between old_notes_rev and new_notes_rev (Task B / owner item 5).
HIST_MAIN_SHA="$(g rev-parse main)"
g update-ref refs/notes/concrete-writer-hist-old "$WRITER_NOTES_SHA"
g notes --ref=refs/notes/concrete-writer-hist-old add -f -m "pre-contract historical note on landed commit" "$HIST_MAIN_SHA" 2>/dev/null
HIST_OLD_NOTES_SHA="$(g rev-parse refs/notes/concrete-writer-hist-old)"
g update-ref refs/notes/concrete-writer-hist-new "$HIST_OLD_NOTES_SHA"
g notes --ref=refs/notes/concrete-writer-hist-new add -f -m "$BF_R2_APPENDED_NOTE" "$BF_R2" 2>/dev/null
hook_out="$(wnote_hook "$(g rev-parse refs/notes/concrete-writer-hist-new)" "$HIST_OLD_NOTES_SHA")"; rc=$?
check "hook does not re-judge an unchanged historical pre-contract writer note on a past landed commit when pushing new notes (exit 0, got $rc)" test "$rc" -eq 0
g update-ref -d refs/notes/concrete-writer-hist-old 2>/dev/null || true
g update-ref -d refs/notes/concrete-writer-hist-new 2>/dev/null || true
hook_out="$(wnote_hook "$ABH_SHA" "$Z")"; rc=$?
check "hook refuses a source commit pushed to refs/notes/concrete-writer (exit 1, got $rc)" \
  bash -c '[ "$1" -eq 1 ] && grep -q "invalid refs/notes/concrete-writer" <<<"$2"' _ "$rc" "$hook_out"
hook_out="$(wnote_hook "$Z" "$WRITER_NOTES_SHA")"; rc=$?
check "hook refuses deleting refs/notes/concrete-writer (exit 1, got $rc)" \
  bash -c '[ "$1" -eq 1 ] && grep -q "refusing to delete refs/notes/concrete-writer" <<<"$2"' _ "$rc" "$hook_out"
# Round 9 controls:
# 1. CONCRETE_SKIP_GATES=1 never bypasses refs/notes/concrete-writer validation or deletion refusal:
#    rollback, deletion, and re-signing attempts are all refused under CONCRETE_SKIP_GATES=1.
wnote_hook_skip() { (cd "$W" && printf 'refs/notes/concrete-writer %s refs/notes/concrete-writer %s\n' "$1" "${2:-$Z}" | CONCRETE_SKIP_GATES=1 bash "$HOOK" 2>&1); }
# Prepare N2 on BF_R2 where a signed correction attributes BF_R2 to codex (so codex cannot review BF_R4):
g update-ref refs/notes/concrete-writer "$WRITER_NOTES_SHA"
backfill_writer "$BF_R2" codex 2 backfill-kimi-agy "$BF_R1" "$BF_R2" "correction naming codex" \
  "$BF_R2_EID" "codex co-wrote BF_R2"
N2_CODEX_NOTES_SHA="$(g rev-parse refs/notes/concrete-writer)"
# Also build N3_ROLLBACK (parent N2_CODEX_NOTES_SHA, rolling BF_R2 back to its 1-stanza note):
g update-ref refs/notes/concrete-writer-r9-rollback "$N2_CODEX_NOTES_SHA"
g notes --ref=refs/notes/concrete-writer-r9-rollback add -f -m "$BF_R2_OLD_NOTE" "$BF_R2" 2>/dev/null
N3_ROLLBACK_SHA="$(g rev-parse refs/notes/concrete-writer-r9-rollback)"
# Rollback through skip switch (both rewinding N2 -> N1 and pushing N3_ROLLBACK):
hook_out="$(wnote_hook_skip "$WRITER_NOTES_SHA" "$N2_CODEX_NOTES_SHA")"; rc=$?
check "skip-switch control (rollback N2->N1): CONCRETE_SKIP_GATES=1 refuses rolling back refs/notes/concrete-writer to an earlier commit (exit 1, got $rc)" \
  bash -c '[ "$1" -eq 1 ] && grep -q "rewrites or drops existing writer-log entry" <<<"$2"' _ "$rc" "$hook_out"
hook_out="$(wnote_hook_skip "$N3_ROLLBACK_SHA" "$N2_CODEX_NOTES_SHA")"; rc=$?
check "skip-switch control (rollback commit N3): CONCRETE_SKIP_GATES=1 refuses a commit that drops a correction stanza (exit 1, got $rc)" \
  bash -c '[ "$1" -eq 1 ] && grep -q "rewrites or drops existing writer-log entry" <<<"$2"' _ "$rc" "$hook_out"
hook_out="$(wnote_hook_skip "$N3_ROLLBACK_SHA" "$Z")"; rc=$?
check "skip-switch control (ancestor continuity on rollback commit N3): CONCRETE_SKIP_GATES=1 refuses N3 even against 0000... sentinel (exit 1, got $rc)" \
  bash -c '[ "$1" -eq 1 ] && grep -q "rewrites or drops existing writer-log entry" <<<"$2"' _ "$rc" "$hook_out"
# Deletion through skip switch (both deleting an entry in N3_DEL and deleting the ref):
hook_out="$(wnote_hook_skip "$(g rev-parse refs/notes/concrete-writer-del)" "$WRITER_NOTES_SHA")"; rc=$?
check "skip-switch control (deletion of note entry): CONCRETE_SKIP_GATES=1 refuses deleting a commit note (exit 1, got $rc)" \
  bash -c '[ "$1" -eq 1 ] && grep -q "deletes existing writer-log entry" <<<"$2"' _ "$rc" "$hook_out"
hook_out="$(wnote_hook_skip "$(g rev-parse refs/notes/concrete-writer-del)" "$Z")"; rc=$?
check "skip-switch control (ancestor continuity on deleted note entry): CONCRETE_SKIP_GATES=1 refuses a deletion commit even against 0000... sentinel (exit 1, got $rc)" \
  bash -c '[ "$1" -eq 1 ] && grep -q "deletes existing writer-log entry" <<<"$2"' _ "$rc" "$hook_out"
hook_out="$(wnote_hook_skip "$Z" "$N2_CODEX_NOTES_SHA")"; rc=$?
check "skip-switch control (deletion of ref): CONCRETE_SKIP_GATES=1 refuses deleting refs/notes/concrete-writer (exit 1, got $rc)" \
  bash -c '[ "$1" -eq 1 ] && grep -q "refusing to delete refs/notes/concrete-writer" <<<"$2"' _ "$rc" "$hook_out"
# Re-signing through skip switch (both against old_notes_rev and against 0000... sentinel via ancestor continuity):
hook_out="$(wnote_hook_skip "$(g rev-parse refs/notes/concrete-writer-replace)" "$WRITER_NOTES_SHA")"; rc=$?
check "skip-switch control (re-signing entry): CONCRETE_SKIP_GATES=1 refuses a re-signed replacement entry (exit 1, got $rc)" \
  bash -c '[ "$1" -eq 1 ] && grep -q "rewrites or drops existing writer-log entry" <<<"$2"' _ "$rc" "$hook_out"
hook_out="$(wnote_hook_skip "$(g rev-parse refs/notes/concrete-writer-replace)" "$Z")"; rc=$?
check "skip-switch control (ancestor continuity on re-signed entry): CONCRETE_SKIP_GATES=1 refuses a re-signed commit even against 0000... sentinel (exit 1, got $rc)" \
  bash -c '[ "$1" -eq 1 ] && grep -q "rewrites or drops existing writer-log entry" <<<"$2"' _ "$rc" "$hook_out"
# notes.sh put writer refusal control:
notes_put_out="$(cd "$W" && bash "$ROOT_DIR/scripts/land/notes.sh" put writer "$BF_R2" model=claude 2>&1)"; rc=$?
check "notes.sh put refuses writer notes and directs callers to the append-only writer_provenance.py API (exit 1, got $rc)" \
  bash -c '[ "$1" -eq 1 ] && grep -q "writer notes are signed and append-only" <<<"$2"' _ "$rc" "$notes_put_out"
# 2. Landing / verify.sh --required-base continuity controls for rollback, deletion, and re-signing:
#    With N2_CODEX_NOTES_SHA active (BF_R2 corrected to codex), a clean review note by reviewer=codex
#    on BF_R4 is self-review and must be refused even if someone attempts rollback, deletion, or re-signing.
g update-ref refs/notes/concrete-writer "$N2_CODEX_NOTES_SHA"
g push -q -f origin "refs/notes/concrete-writer:refs/notes/concrete-writer"
g notes --ref=refs/notes/concrete-review add -f -m "$(printf 'verdict: clean\nrounds: 1\nreviewer: codex\nwriter: kimi,agy\nutc: 2026-11-08T00:00:00Z\nbase: %s\npieces: 1\nrepository: readable' "$BF_BASE")" "$BF_R4"
g push -q -f origin "refs/notes/concrete-review:refs/notes/concrete-review"
# Verify baseline: N2_CODEX_NOTES_SHA refuses codex as reviewer on BF_R4:
v_n2_out="$(cd "$W" && bash "$VERIFY" --fetch --required-base "$BF_BASE" "$BF_R4" 2>&1)"; rc=$?
check "verify.sh --fetch --required-base refuses reviewer codex when BF_R2 has a signed correction naming codex (exit 1, got $rc)" test "$rc" -eq 1
# Landing/verify Rollback control A (rewind origin's refs/notes/concrete-writer from N2 to N1):
g push -q -f --no-verify origin "$WRITER_NOTES_SHA:refs/notes/concrete-writer"
v_rb1_out="$(cd "$W" && bash "$VERIFY" --fetch --required-base "$BF_BASE" "$BF_R4" 2>&1)"; rc=$?
check "landing/verify control (rollback N2->N1 on remote): verify.sh --fetch --required-base refuses rolled-back writer history (exit 1, got $rc)" \
  bash -c '[ "$1" -eq 1 ] && grep -q "rewrites or drops existing writer-log entry" <<<"$2"' _ "$rc" "$v_rb1_out"
# Landing/verify Rollback control B (commit N3_ROLLBACK on top of N2):
g push -q -f --no-verify origin "$N3_ROLLBACK_SHA:refs/notes/concrete-writer"
v_rb2_out="$(cd "$W" && bash "$VERIFY" --fetch --required-base "$BF_BASE" "$BF_R4" 2>&1)"; rc=$?
check "landing/verify control (rollback commit N3 on remote): verify.sh --fetch --required-base refuses dropped stanza (exit 1, got $rc)" \
  bash -c '[ "$1" -eq 1 ] && grep -q "rewrites or drops existing writer-log entry" <<<"$2"' _ "$rc" "$v_rb2_out"
g update-ref refs/notes/concrete-writer "$N3_ROLLBACK_SHA"
v_rb3_out="$(cd "$W" && bash "$VERIFY" --required-base "$BF_BASE" "$BF_R4" 2>&1)"; rc=$?
check "landing/verify control (local rollback commit N3): verify.sh --required-base refuses dropped stanza via ancestor continuity (exit 1, got $rc)" \
  bash -c '[ "$1" -eq 1 ] && grep -q "rewrites or drops existing writer-log entry" <<<"$2"' _ "$rc" "$v_rb3_out"
# Landing/verify Deletion control A (delete BF_R2's note on top of N2):
g update-ref refs/notes/concrete-writer "$N2_CODEX_NOTES_SHA"
g update-ref refs/notes/concrete-writer-r9-del "$N2_CODEX_NOTES_SHA"
g notes --ref=refs/notes/concrete-writer-r9-del remove "$BF_R2" 2>/dev/null
N3_DEL_R9_SHA="$(g rev-parse refs/notes/concrete-writer-r9-del)"
g push -q -f --no-verify origin "$N3_DEL_R9_SHA:refs/notes/concrete-writer"
v_del1_out="$(cd "$W" && bash "$VERIFY" --fetch --required-base "$BF_BASE" "$BF_R4" 2>&1)"; rc=$?
check "landing/verify control (note deletion on remote): verify.sh --fetch --required-base refuses deleted writer note (exit 1, got $rc)" \
  bash -c '[ "$1" -eq 1 ] && grep -q "deletes existing writer-log entry" <<<"$2"' _ "$rc" "$v_del1_out"
# Landing/verify Deletion control B (delete refs/notes/concrete-writer on remote, then replay older signed history):
git -C "$T/origin.git" update-ref -d refs/notes/concrete-writer
v_del2_out="$(cd "$W" && bash "$VERIFY" --fetch --required-base "$BF_BASE" "$BF_R4" 2>&1)"; rc=$?
check "landing/verify control (ref deletion on remote): verify.sh --fetch --required-base refuses when remote writer-notes ref is deleted and preserves local checkpoints (exit 1, got $rc)" \
  bash -c '[ "$1" -eq 1 ] && grep -q "prior writer-log history was recorded" <<<"$2" && [ "$(git -C "$3" rev-parse refs/notes/concrete-writer)" = "$4" ] && [ "$(git -C "$3" rev-parse refs/notes/remote/concrete-writer)" = "$4" ]' _ "$rc" "$v_del2_out" "$W" "$N2_CODEX_NOTES_SHA"
# Extended Deletion control B (delete-then-replay / resurrection refusal):
# Without touching local refs, restore the older signed 1-stanza history ($WRITER_NOTES_SHA) on origin;
# verify.sh --fetch must still refuse because the local checkpoint at $N2_CODEX_NOTES_SHA was preserved.
g push -q -f --no-verify origin "$WRITER_NOTES_SHA:refs/notes/concrete-writer"
v_del2_replay_out="$(cd "$W" && bash "$VERIFY" --fetch --required-base "$BF_BASE" "$BF_R4" 2>&1)"; rc=$?
check "landing/verify control (delete-then-replay resurrection refusal): restoring older signed history after remote ref deletion is refused by verify.sh --fetch (exit 1, got $rc)" \
  bash -c '[ "$1" -eq 1 ] && grep -q "rewrites or drops existing writer-log entry" <<<"$2"' _ "$rc" "$v_del2_replay_out"
# Explicit agy -> kimi correction delete-then-replay control (reproducing the round-9 Codex finding):
# Record agy on DTR_C (DTR_N1), append signed correction naming kimi (DTR_N2), and supply a Kimi clean review note.
g checkout -q main && g checkout -qb dtr-agy-kimi
commit_unstamped dtr.txt "dtr" "dtr commit"
DTR_C="$(g rev-parse HEAD)"
DTR_BASE="$(g merge-base HEAD main)"
g update-ref refs/notes/concrete-writer "$N2_CODEX_NOTES_SHA"
backfill_writer "$DTR_C" agy 1 dtr-agy-kimi "$DTR_BASE" "$DTR_C" "initial agy backfill on DTR_C"
DTR_N1="$(g rev-parse refs/notes/concrete-writer)"
DTR_EID1="$(g notes --ref=refs/notes/concrete-writer show "$DTR_C" | sed -n 's/^entry-id: //p')"
backfill_writer "$DTR_C" kimi 2 dtr-agy-kimi "$DTR_BASE" "$DTR_C" "correction naming kimi on DTR_C" \
  "$DTR_EID1" "kimi co-wrote DTR_C"
DTR_N2="$(g rev-parse refs/notes/concrete-writer)"
g push -q -f --no-verify origin "$DTR_N2:refs/notes/concrete-writer"
g notes --ref=refs/notes/concrete-review add -f -m "$(printf 'verdict: clean\nrounds: 1\nreviewer: kimi\nwriter: agy\nutc: 2026-11-08T00:00:00Z\nbase: %s\npieces: 1\nrepository: readable' "$DTR_BASE")" "$DTR_C"
g push -q -f origin "refs/notes/concrete-review:refs/notes/concrete-review"
dtr_base_out="$(cd "$W" && bash "$VERIFY" --fetch --required-base "$DTR_BASE" "$DTR_C" 2>&1)"; rc=$?
check "delete-then-replay (agy->kimi): baseline verify.sh --fetch rejects Kimi self-review (exit 1, got $rc)" test "$rc" -eq 1
git -C "$T/origin.git" update-ref -d refs/notes/concrete-writer
dtr_del_out="$(cd "$W" && bash "$VERIFY" --fetch --required-base "$DTR_BASE" "$DTR_C" 2>&1)"; rc=$?
check "delete-then-replay (agy->kimi): deleting remote writer ref is rejected and does not erase local checkpoint (exit 1, got $rc)" \
  bash -c '[ "$1" -eq 1 ] && grep -q "prior writer-log history was recorded" <<<"$2" && [ "$(git -C "$3" rev-parse refs/notes/concrete-writer)" = "$4" ]' _ "$rc" "$dtr_del_out" "$W" "$DTR_N2"
g push -q -f --no-verify origin "$DTR_N1:refs/notes/concrete-writer"
dtr_replay_out="$(cd "$W" && bash "$VERIFY" --fetch --required-base "$DTR_BASE" "$DTR_C" 2>&1)"; rc=$?
check "delete-then-replay (agy->kimi): restoring older signed agy-only history is rejected by verify.sh --fetch (exit 1, got $rc)" \
  bash -c '[ "$1" -eq 1 ] && grep -q "rewrites or drops existing writer-log entry" <<<"$2"' _ "$rc" "$dtr_replay_out"
g notes --ref=refs/notes/concrete-review remove "$DTR_C" 2>/dev/null || true
g update-ref refs/notes/concrete-writer "$N2_CODEX_NOTES_SHA"
g update-ref refs/notes/remote/concrete-writer "$N2_CODEX_NOTES_SHA"
git -C "$T/origin.git" update-ref refs/notes/concrete-writer "$N2_CODEX_NOTES_SHA"
# Landing/verify Re-signing control (replace BF_R2's note with a freshly signed 1-stanza note on top of N2):
g update-ref refs/notes/concrete-writer "$N2_CODEX_NOTES_SHA"
sed "s/^evidence: .*/evidence: re-signed single stanza/" <<<"$BF_R2_OLD_NOTE" >"$T/resigned-bf-r2-raw"
sign_raw_stanza "$T/resigned-bf-r2-raw" "$T/resigned-bf-r2"
g update-ref refs/notes/concrete-writer-r9-resign "$N2_CODEX_NOTES_SHA"
g notes --ref=refs/notes/concrete-writer-r9-resign add -f -F "$T/resigned-bf-r2" "$BF_R2" 2>/dev/null
N3_RESIGN_R9_SHA="$(g rev-parse refs/notes/concrete-writer-r9-resign)"
g push -q -f --no-verify origin "$N3_RESIGN_R9_SHA:refs/notes/concrete-writer"
v_res1_out="$(cd "$W" && bash "$VERIFY" --fetch --required-base "$BF_BASE" "$BF_R4" 2>&1)"; rc=$?
check "landing/verify control (re-signing on remote): verify.sh --fetch --required-base refuses re-signed replacement note (exit 1, got $rc)" \
  bash -c '[ "$1" -eq 1 ] && grep -q "rewrites or drops existing writer-log entry" <<<"$2"' _ "$rc" "$v_res1_out"
g update-ref refs/notes/concrete-writer "$N3_RESIGN_R9_SHA"
v_res2_out="$(cd "$W" && bash "$VERIFY" --required-base "$BF_BASE" "$BF_R4" 2>&1)"; rc=$?
check "landing/verify control (local re-signing commit): verify.sh --required-base refuses re-signed replacement note via ancestor continuity (exit 1, got $rc)" \
  bash -c '[ "$1" -eq 1 ] && grep -q "rewrites or drops existing writer-log entry" <<<"$2"' _ "$rc" "$v_res2_out"
# Restore clean state on $W and $T/origin.git:
g notes --ref=refs/notes/concrete-review remove "$BF_R4" 2>/dev/null || true
g push -q -f origin "refs/notes/concrete-review:refs/notes/concrete-review"
g update-ref refs/notes/concrete-writer "$WRITER_NOTES_SHA"
g update-ref -d refs/notes/remote/concrete-writer 2>/dev/null || true
git -C "$T/origin.git" update-ref refs/notes/concrete-writer "$WRITER_NOTES_SHA"
g update-ref -d refs/notes/concrete-writer-bad 2>/dev/null || true
g update-ref -d refs/notes/concrete-writer-del 2>/dev/null || true
g update-ref -d refs/notes/concrete-writer-replace 2>/dev/null || true
g update-ref -d refs/notes/concrete-writer-r9-rollback 2>/dev/null || true
g update-ref -d refs/notes/concrete-writer-r9-del 2>/dev/null || true
g update-ref -d refs/notes/concrete-writer-r9-resign 2>/dev/null || true
# Owner sign-off refs: only a record (parentless, its subject's 40-hex name, exactly the seven files)
# takes the notes tier. Anything else takes the full tier, which a linked worktree refuses before it
# runs anything: that refusal is how these legs see the tier without running the gates.
SO_BLOB="$(printf 'x\n' | g hash-object -w --stdin)"
SO_TREE="$(printf '100644 blob %s\tagy-request.txt\n100644 blob %s\tagy.txt\n100644 blob %s\tcodex-request.txt\n100644 blob %s\tcodex.txt\n100644 blob %s\tkimi-request.txt\n100644 blob %s\tkimi.txt\n100644 blob %s\trecord.txt\n' "$SO_BLOB" "$SO_BLOB" "$SO_BLOB" "$SO_BLOB" "$SO_BLOB" "$SO_BLOB" "$SO_BLOB" | g mktree)"
SO_REC="$(g commit-tree "$SO_TREE" -m record)"; SO_SUBJ="$(g rev-parse HEAD)"
SO_INCOMPLETE_TREE="$(printf '100644 blob %s\tagy.txt\n100644 blob %s\tcodex.txt\n100644 blob %s\tkimi.txt\n100644 blob %s\trecord.txt\n' "$SO_BLOB" "$SO_BLOB" "$SO_BLOB" "$SO_BLOB" | g mktree)"
SO_INCOMPLETE_REC="$(g commit-tree "$SO_INCOMPLETE_TREE" -m incomplete-record)"
g worktree add -q --detach "$T/so-wt" HEAD
so_hook(){ (cd "$T/so-wt" && printf '%s %s %s %s\n' "$1" "$2" "$1" "$Z" | CONCRETE_GATES_SHELL= bash "$HOOK" origin x 2>&1); }
hook_out="$(so_hook "refs/owner-signoff/$SO_SUBJ" "$SO_REC")"; rc=$?
check "a seven-file owner sign-off record takes the review-notes tier (exit $rc)" \
  bash -c "[ $rc -eq 0 ] && grep -q 'review-notes push' <<<'$hook_out'"
hook_out="$(so_hook "refs/owner-signoff/$SO_SUBJ" "$SO_INCOMPLETE_REC")"; rc=$?
check "an incomplete sign-off tree missing request files takes the full tier (exit $rc)" \
  bash -c "[ $rc -eq 1 ] && grep -q 'is a linked worktree' <<<'$hook_out'"
hook_out="$(so_hook "refs/owner-signoff/$SO_SUBJ" "$SO_SUBJ")"; rc=$?
check "a source commit pushed as a sign-off takes the full tier (exit $rc)" \
  bash -c "[ $rc -eq 1 ] && grep -q 'is a linked worktree' <<<'$hook_out'"
SO_DTREE="$(printf '100644 blob %s\tsrc.lean\n' "$SO_BLOB" | g mktree)"
SO_DIRS="$(g commit-tree "$(printf '040000 tree %s\tagy-request.txt\n040000 tree %s\tagy.txt\n040000 tree %s\tcodex-request.txt\n040000 tree %s\tcodex.txt\n040000 tree %s\tkimi-request.txt\n040000 tree %s\tkimi.txt\n040000 tree %s\trecord.txt\n' "$SO_DTREE" "$SO_DTREE" "$SO_DTREE" "$SO_DTREE" "$SO_DTREE" "$SO_DTREE" "$SO_DTREE" | g mktree)" -m dirs)"
hook_out="$(so_hook "refs/owner-signoff/$SO_SUBJ" "$SO_DIRS")"; rc=$?
check "seven directories named like a record's files take the full tier (exit $rc)" \
  bash -c "[ $rc -eq 1 ] && grep -q 'is a linked worktree' <<<'$hook_out'"
hook_out="$(so_hook "refs/owner-signoff/not-a-sha" "$SO_REC")"; rc=$?
check "a record under a name that is no SHA takes the full tier (exit $rc)" \
  bash -c "[ $rc -eq 1 ] && grep -q 'is a linked worktree' <<<'$hook_out'"
g worktree remove --force "$T/so-wt"
hook_out="$(cd "$W" && printf 'refs/heads/mixed %s refs/heads/wip/x %s\nrefs/notes/concrete-review %s refs/notes/concrete-review %s\n' \
  "$(g rev-parse HEAD)" "$Z" "$NOTES_SHA" "$Z" | bash "$HOOK" 2>&1)"; rc=$?
check "notes alongside a wip push take the wip tier (exit $rc)" \
  bash -c "[ $rc -eq 0 ] && grep -q 'wip/\* backup push' <<<'$hook_out'"

echo "=== splitting by file group and size ==="
reset_fake
g checkout -q main && g checkout -qb split
mkdir -p "$W/Concrete" "$W/tests" "$W/docs" "$W/scripts"
for i in 1 2 3; do printf 'def f%s := %s\n' "$i" "$i" >"$W/Concrete/M$i.lean"; done
printf 'test\n' >"$W/tests/t.con"
printf 'doc\n' >"$W/docs/d.md"
printf 'echo hi\n' >"$W/scripts/s.sh"
seq 1 400 | sed 's/^/big line /' >"$W/Concrete/Big.lean"
g add -A && g commit -qm "spread change"
stamp_writer claude 1 split
REVIEW_PIECE_BYTES=2000 review >"$T/out5" 2>&1; rc=$?
check "split review is clean (exit $rc)" test "$rc" -eq 0
K="$(field pieces)"
check "several pieces ($K)" test "$K" -ge 5
check "one call per piece" test "$(calls)" -eq "$K"
mixed=0
while read -r line; do
  files="${line##*files=}"
  groups="$(tr ',' '\n' <<<"$files" | sed '/^$/d' | awk -F/ '{ print $1 }' | sort -u | wc -l | tr -d ' ')"
  [ "$groups" -le 1 ] || mixed=1
done <"$FAKE_LOG/calls"
check "no piece mixes file groups" test "$mixed" -eq 0
over="$(awk '{ sub("bytes=", "", $4); if ($4 + 0 > 2000) print }' "$FAKE_LOG/calls")"
check "every piece within the size limit${over:+: $over}" test -z "$over"
check "the big file is split across pieces" test "$(grep -c 'files=Concrete/Big.lean,$' "$FAKE_LOG/calls")" -ge 2
check "compiler pieces come first" bash -c "head -1 '$FAKE_LOG/calls' | has_match 'files=Concrete/'"
bigpart="$(grep -l '^diff --git a/Concrete/Big.lean' "$FAKE_LOG"/stdin.* | awk 'NR==2')"
check "each split part repeats the file header and starts a hunk" \
  bash -c "head -1 '$bigpart' | has_match '^diff --git' && grep -q '^@@' '$bigpart'"

g diff "$(g merge-base HEAD main)" HEAD >"$T/split.diff"
total="$(wc -c <"$T/split.diff" | tr -d ' ')"
n="$(python3 "$SPLIT" "$T/split.diff" "$T/sp3" $(( (total + 2) / 3 )))"
check "--pieces N sizing: a third of the diff gives about three pieces ($n)" test "$n" -ge 3 -a "$n" -le 6
n1p="$(python3 "$SPLIT" "$T/split.diff" "$T/sp1" "$total")"
check "a limit of the whole diff still separates the four groups ($n1p)" test "$n1p" -eq 4
# Hunk headers are excluded: a hunk cut in parts gets renumbered headers.
cat "$T"/sp3/piece-*.diff | grep -v '^@@' | sort >"$T/sp3.sorted"
grep -v '^@@' "$T/split.diff" | sort >"$T/orig.sorted"
check "split loses no diff line" bash -c "comm -23 '$T/orig.sorted' '$T/sp3.sorted' | has_match . && exit 1 || exit 0"
reset_fake
g checkout -q main && g checkout -qb pieces-n
commit_file tests/a.con "$(seq 1 300)" "tests only"
review --pieces 3 >/dev/null 2>&1
p3="$(field pieces)"
check "--pieces 3 on one file gives three pieces ($p3)" test "$p3" -eq 3
# Each part's @@ header must match its own body, and the parts must continue
# one another: part k+1 starts where part k ended, on both sides.
cat >"$T/hunks.py" <<'EOF'
import re, sys
expect_old = expect_new = None
for path in sys.argv[1:]:
    hunks = [l for l in open(path) if l.startswith("@@")]
    if len(hunks) != 1:
        sys.exit("%s: want one hunk, got %d" % (path, len(hunks)))
    m = re.match(r"@@ -(\d+),(\d+) \+(\d+),(\d+) @@", hunks[0])
    if not m:
        sys.exit("%s: unnumbered header %r" % (path, hunks[0]))
    o, oc, n, nc = map(int, m.groups())
    body = open(path).read().split(hunks[0], 1)[1].splitlines()
    got_o = sum(1 for l in body if l[:1] in (" ", "-"))
    got_n = sum(1 for l in body if l[:1] in (" ", "+"))
    if (got_o, got_n) != (oc, nc):
        sys.exit("%s: header counts %d,%d but body has %d,%d" % (path, oc, nc, got_o, got_n))
    if expect_new is not None and (o, n) != (expect_old, expect_new):
        sys.exit("%s: starts at -%d +%d, previous part ended at -%d +%d" % (path, o, n, expect_old, expect_new))
    expect_old, expect_new = o + oc, n + nc
if expect_new != 301:
    sys.exit("parts end at +%s, want +301 (300 lines)" % expect_new)
EOF
check "split parts carry correct, contiguous @@ headers" \
  python3 "$T/hunks.py" $(ls "$FAKE_LOG"/stdin.* | sort -t. -k2 -n)

echo "=== the round cap ==="
reset_fake
g checkout -q main && g checkout -qb capped
commit_file a.txt "MARK_FINDING one" "c1"
review --max-rounds 2 >/dev/null 2>&1; rc=$?
check "round 1 of 2: findings (exit $rc)" test "$rc" -eq 1
commit_file a.txt "MARK_FINDING two" "c2"
review --max-rounds 2 >"$T/cap" 2>&1; rc=$?
check "round 2 of 2 with findings: cap exit 3 (got $rc)" test "$rc" -eq 3
check "cap message says split or escalate" grep -q "Split the branch" "$T/cap"
check "cap message lists the open findings" grep -q "counter is never reset" "$T/cap"
check "round 2 note was still written" test "$(field rounds)/$(field verdict)" = "2/findings"
before="$(calls)"
commit_file a.txt "fixed" "c3"
review --max-rounds 2 >"$T/cap2" 2>&1; rc=$?
check "no round 3: refused with exit 3 (got $rc)" test "$rc" -eq 3
check "and no reviewer call" test "$(calls)" -eq "$before"
check "no note written on the refused round" bash -c "! git -C '$W' notes --ref=refs/notes/concrete-review show HEAD >/dev/null 2>&1"

echo "=== notes push: non-fast-forward is fetched, merged and retried ==="
git clone -q "$T/origin.git" "$T/other" 2>/dev/null
git -C "$T/other" fetch -q origin "+refs/notes/concrete-review:refs/notes/concrete-review"
OTHER_SHA="$(git -C "$T/other" rev-parse HEAD)"
git -C "$T/other" notes --ref=refs/notes/concrete-review add -f -m "verdict: clean
rounds: 1
reviewer: codex
writer: claude
utc: 2026-10-22T00:00:00Z
base: $OTHER_SHA
pieces: 1
repository: readable" "$OTHER_SHA"
git -C "$T/other" push -q origin refs/notes/concrete-review
reset_fake
g checkout -q main && g checkout -qb nonff
commit_file z.txt "z" "z"
# Our local notes ref lacks the other writer's note: the start-of-round sync
# must merge it, or the push below would be a non-fast-forward.
review >"$T/out6" 2>&1; rc=$?
check "review after a concurrent notes push succeeds (exit $rc)" test "$rc" -eq 0
check "origin keeps the other writer's note" \
  bash -c "git -C '$T/origin.git' notes --ref=refs/notes/concrete-review show $OTHER_SHA | has_match 'verdict: clean'"
check "origin has our note" \
  bash -c "git -C '$T/origin.git' notes --ref=refs/notes/concrete-review show $(g rev-parse HEAD) | has_match 'verdict: clean'"
# Now force the non-fast-forward at push time: another writer pushes between
# our sync and our push (the fake reviewer does it while it "reviews").
git -C "$T/other" fetch -q origin "+refs/notes/concrete-review:refs/notes/concrete-review"
cat >"$T/bin/codex-race" <<EOF
#!/usr/bin/env bash
git -C "$T/other" notes --ref=refs/notes/concrete-review add -f -m "race note" "$OTHER_SHA"
git -C "$T/other" push -q origin refs/notes/concrete-review
exec "$T/bin/codex" "\$@"
EOF
chmod +x "$T/bin/codex-race"
reset_fake
commit_file z2.txt "z2" "z2"
REVIEW_CODEX_BIN="$T/bin/codex-race" review >"$T/out7" 2>&1; rc=$?
check "push rejected once, then merged and pushed (exit $rc)" test "$rc" -eq 0
check "the retry path ran" grep -q "rejected; fetching and merging" "$T/out7"
check "origin holds the racing note" \
  bash -c "git -C '$T/origin.git' notes --ref=refs/notes/concrete-review show $OTHER_SHA | has_match 'race note'"
check "origin holds our note" \
  bash -c "git -C '$T/origin.git' notes --ref=refs/notes/concrete-review show $(g rev-parse HEAD) | has_match 'verdict: clean'"

echo "=== notes push: any other refusal stops at once (exit 6) ==="
printf '#!/bin/sh\necho "remote policy says no" >&2\nexit 1\n' >"$T/origin.git/hooks/pre-receive"
chmod +x "$T/origin.git/hooks/pre-receive"
reset_fake
commit_file z3.txt "z3" "z3"
review >"$T/out9" 2>&1; rc=$?
rm -f "$T/origin.git/hooks/pre-receive"
check "a refused (not non-fast-forward) push exits 6 (got $rc)" test "$rc" -eq 6
check "and is not retried as if it were a race" bash -c "! grep -q 'rejected; fetching and merging' '$T/out9'"
check "the note is still recorded locally" test "$(field verdict)" = clean

echo "=== usage errors ==="
review --pieces zero >/dev/null 2>&1; rc=$?
check "bad --pieces: exit 2 (got $rc)" test "$rc" -eq 2
review --bogus >/dev/null 2>&1; rc=$?
check "unknown flag: exit 2 (got $rc)" test "$rc" -eq 2

echo "=== source tree hygiene (no Python bytecode written into repository) ==="
( unset PYTHONDONTWRITEBYTECODE; sign_raw_stanza "$T/good-agy-log" "$T/bytecode-check-stanza" )
check "sign_raw_stanza and review_tool leave scripts/review/__pycache__ absent" test ! -e "$ROOT_DIR/scripts/review/__pycache__"
check "review_tool check run leaves the source tree byte-identical (no tracked, untracked or ignored __pycache__/*.pyc changes)" \
  verify_source_tree_unchanged "$T/root.before" "$ROOT_SNAP_BEFORE_RC" "$ROOT_DIR" "$T/root.after"

# Negative controls for snapshot_source_tree / verify_source_tree_unchanged in both a git checkout and an archive-shaped tree:
git init -q "$T/snap-git"
mkdir -p "$T/snap-git/sub"
printf 'initial content\n' >"$T/snap-git/existing.txt"
printf 'sub content\n' >"$T/snap-git/sub/file.txt"
ln -s existing.txt "$T/snap-git/link.txt"
git -C "$T/snap-git" add -A && git -C "$T/snap-git" commit -qm "snap base"
snap_git_rc=0
snapshot_source_tree "$T/snap-git" >"$T/snap-git.before" 2>"$T/snap-git.err" || snap_git_rc=$?
check "snapshot_source_tree succeeds on a git checkout with no stderr (exit 0, got $snap_git_rc)" \
  bash -c '[ "$1" -eq 0 ] && [ -s "$2" ] && [ ! -s "$3" ]' _ "$snap_git_rc" "$T/snap-git.before" "$T/snap-git.err"
check "verify_source_tree_unchanged passes on an unchanged git checkout" \
  verify_source_tree_unchanged "$T/snap-git.before" "$snap_git_rc" "$T/snap-git" "$T/snap-git.after"
printf 'modified content\n' >"$T/snap-git/existing.txt"
check "negative control (git checkout): changing an existing file's contents fails verify_source_tree_unchanged" \
  verify_source_tree_changed "$T/snap-git.before" "$snap_git_rc" "$T/snap-git" "$T/snap-git.after"
printf 'initial content\n' >"$T/snap-git/existing.txt"
chmod +x "$T/snap-git/existing.txt"
check "negative control (git checkout): changing an existing file's mode fails verify_source_tree_unchanged" \
  verify_source_tree_changed "$T/snap-git.before" "$snap_git_rc" "$T/snap-git" "$T/snap-git.after"
chmod -x "$T/snap-git/existing.txt"
ln -sf sub/file.txt "$T/snap-git/link.txt"
check "negative control (git checkout): changing a symlink target fails verify_source_tree_unchanged" \
  verify_source_tree_changed "$T/snap-git.before" "$snap_git_rc" "$T/snap-git" "$T/snap-git.after"
ln -sf existing.txt "$T/snap-git/link.txt"

mkdir -p "$T/snap-archive"
git -C "$T/snap-git" archive HEAD | tar -x -C "$T/snap-archive"
snap_arch_rc=0
snapshot_source_tree "$T/snap-archive" >"$T/snap-arch.before" 2>"$T/snap-arch.err" || snap_arch_rc=$?
check "snapshot_source_tree succeeds on an archive-shaped tree (no .git) with no git errors (exit 0, got $snap_arch_rc)" \
  bash -c '[ "$1" -eq 0 ] && [ -s "$2" ] && [ ! -s "$3" ]' _ "$snap_arch_rc" "$T/snap-arch.before" "$T/snap-arch.err"
check "verify_source_tree_unchanged passes on an unchanged archive-shaped tree" \
  verify_source_tree_unchanged "$T/snap-arch.before" "$snap_arch_rc" "$T/snap-archive" "$T/snap-arch.after"
printf 'modified in archive\n' >"$T/snap-archive/existing.txt"
check "negative control (archive-shaped tree): changing an existing file's contents fails verify_source_tree_unchanged" \
  verify_source_tree_changed "$T/snap-arch.before" "$snap_arch_rc" "$T/snap-archive" "$T/snap-arch.after"
printf 'initial content\n' >"$T/snap-archive/existing.txt"
chmod +x "$T/snap-archive/existing.txt"
check "negative control (archive-shaped tree): changing an existing file's mode fails verify_source_tree_unchanged" \
  verify_source_tree_changed "$T/snap-arch.before" "$snap_arch_rc" "$T/snap-archive" "$T/snap-arch.after"
chmod -x "$T/snap-archive/existing.txt"
ln -sf sub/file.txt "$T/snap-archive/link.txt"
check "negative control (archive-shaped tree): changing a symlink target fails verify_source_tree_unchanged" \
  verify_source_tree_changed "$T/snap-arch.before" "$snap_arch_rc" "$T/snap-archive" "$T/snap-arch.after"
ln -sf existing.txt "$T/snap-archive/link.txt"
check "negative control (snapshot error): verify_source_tree_unchanged fails when before-snapshot or after-snapshot errors" \
  verify_source_tree_changed "$T/snap-arch.before" 1 "$T/snap-archive" "$T/snap-arch.after"
check "negative control (missing directory error): verify_source_tree_unchanged fails when target directory is missing" \
  verify_source_tree_changed "$T/snap-arch.before" 0 "$T/does-not-exist" "$T/snap-arch.after"

echo
echo "check_review_tool: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ] || exit 1
