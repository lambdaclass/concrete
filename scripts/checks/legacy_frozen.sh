#!/usr/bin/env bash
# legacy/ IS FROZEN (ROADMAP R-0558, R-0562).
#
# legacy/ is a byte-for-byte copy of main's tree at the move's base commit, plus the files later
# sweeps moved into it byte for byte. scripts/checks/legacy.lock records that commit and its tree, and
# each sweep; its format, and its one reader, is lib/legacy_lock.py (no code here reads the lock's
# bytes itself). This check holds five things:
#   1. the recorded tree is the recorded commit's tree (legacy/ is that snapshot, nothing else);
#   2. HEAD:legacy differs from it only by deletions and by sweeps: a modified path is refused, and an
#      added path is refused unless a sweep moved it (below);
#   3. each deleted path is covered by at least one RETENTION.tsv row, and every row covering it
#      (its own path or an ancestor directory) has `status` = `resolved` (the row's deletion
#      criterion holds): a resolved parent never releases a child with its own unresolved row.
#      Until RETENTION.tsv has a status column, every deletion is refused.
#   4. SWEEPS (owner, 2026-11-08: "ok to 1"). Each `sweep <commit> <tree>` line of the lock records
#      one move into legacy/: the commit of HEAD's history it took files from, and the legacy/ tree
#      it produced. The trees form a chain, frozen tree -> sweep 1 -> ... -> HEAD:legacy. Each step
#      to a sweep tree may add a path P only if P has the same mode and blob at that sweep's commit
#      and P no longer exists at the root of HEAD; the last step, to HEAD:legacy, adds nothing. Every
#      step refuses an edit of a legacy/ file and admits only the deletions RETENTION.tsv allows, so
#      a swept file, once in a sweep tree, is protected like a frozen one.
#   5. TRUSTED HISTORY. The steps above judge the lock this commit carries, so on their own a rewritten
#      lock line, or a file deleted by one sweep and re-added with other bytes by the next, would pass.
#      History settles both. The trusted history is main's landed states: the first-parent chain of
#      the merge base of HEAD and LEGACY_TRUSTED_REF (default origin/main), each commit read against
#      its first parent. Harnesses pass that merge base explicitly as LEGACY_TRUSTED_REF so a
#      sandbox with different refs judges the same trusted history. Otherwise, use HEAD's own first-parent chain when that ref is absent (on main and on a
#      train head, whose first parent is main, the same thing; on a bare branch tip it is stricter:
#      the branch's own commits count, so a branch that corrected its own lock fails there). A train's member commits are second parents: a line a branch wrote and then
#      corrected before it landed is not trusted history. (a) Every base, tree and sweep line of every trusted version of
#      the lock is still in the lock: lines are only ever added. (b) Every version a legacy/ path ever
#      had in trusted history is the version it has now, if it still exists: a frozen or swept file
#      is never rewritten, by an edit or by deletion and re-addition. A candidate's own commits are
#      not trusted history; they are what the review and this gate judge.
# Negative controls run the same judgment on synthetic trees first, so a judge that accepts
# everything fails here before the real check.
set -uo pipefail
source "$(dirname "${BASH_SOURCE[0]}")/lib/gate_env.sh" || exit 2
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
SELF="$ROOT_DIR/scripts/checks/${BASH_SOURCE[0]##*/}"
cd "$ROOT_DIR"
LOCK=scripts/checks/legacy.lock
LOCKPY="$ROOT_DIR/scripts/checks/lib/legacy_lock.py"
PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }

# checked STEP COMMAND...: preserve semantic diagnostics; execution failures always print a refusal.
# Helpers use stdout as the refusal channel, including when the failed command printed nothing.
checked() {
  local label="$1" output rc; shift
  output="$("$@")"; rc=$?
  [ -z "$output" ] || printf '%s\n' "$output"
  if [ "$rc" -ne 0 ] && { [ "$rc" -ne 1 ] || [ -z "$output" ]; }; then
    printf 'refused: %s failed (exit %s)\n' "$label" "$rc"
  fi
  return "$rc"
}

# judge FROZEN_TREE CURRENT_TREE RETENTION_FILE (in the current repository): prints one problem per line.
judge() { # FROZEN CUR RETENTION ROOT-TREE [SWEEP-COMMIT...]
  local frozen="$1" cur="$2" ret="$3" root="$4"; shift 4
  [ "$frozen" = "$cur" ] && return 0
  # -z: paths arrive raw, never C-quoted, so the ls-tree lookups below see the real path.
  git diff-tree -z -r --no-renames --name-status "$frozen" "$cur" | python3 -I -c '
import sys
with open(sys.argv[1]) as manifest:
    records = [line.rstrip("\n").split("\t") for line in manifest
               if line.strip() and not line.startswith("#")]
header, *records = records
requirements = []
if "path" in header:
    path_column = header.index("path")
    status_column = header.index("status") if "status" in header else None
    for record in records:
        resolved = (status_column is not None and len(record) > status_column
                    and record[status_column].strip() == "resolved")
        requirements.extend((path.rstrip("/"), resolved)
                            for path in record[path_column].split())

def deletion_allowed(path):
    covering = [resolved for prefix, resolved in requirements
                if path == prefix or path.startswith(prefix + "/")]
    return bool(covering) and all(covering)

changes = sys.stdin.buffer.read().split(b"\0")
if changes[-1] == b"":
    changes.pop()
if len(changes) % 2:
    raise ValueError("incomplete diff-tree record")
for offset in range(0, len(changes), 2):
    status = changes[offset].decode("ascii")
    path = changes[offset + 1].decode("utf-8", "surrogateescape")
    if not path.isprintable() or "\n" in path or "\t" in path:
        print(f"{status} legacy/{path!r}: legacy/ admits no path with control characters")
    elif status == "A":
        print(f"ADD\t{path}")
    elif status == "D":
        if not deletion_allowed(path):
            print(f"D legacy/{path}: no resolved RETENTION.tsv row allows this deletion")
    else:
        print(f"{status} legacy/{path}: legacy/ admits no modification")
' "$ret" | while IFS= read -r line || [ -n "$line" ]; do
    case "$line" in ADD$'\t'*) ;; *) printf '%s\n' "$line"; continue ;; esac
    local path="${line#ADD$'\t'}" want s entry found=""
    local -x GIT_LITERAL_PATHSPECS=1 # a path is a path: ":(literal)p/x" must not resolve to p/x
    want="$(git ls-tree "$cur" -- "$path" | cut -f1)" || { echo "refused: judge added-path lookup failed"; continue; }
    [ -n "$want" ] || { echo "A legacy/$path: cannot resolve the added path; refused"; continue; }
    entry="$(git ls-tree "$root" -- "$path")" || { echo "refused: judge root lookup failed"; continue; }
    if [ -n "$entry" ]; then
      echo "A legacy/$path: a swept file must no longer exist at the root, and $path does"; continue
    fi
    for s in "$@"; do
      entry="$(git ls-tree "$s" -- "$path" | cut -f1)" || { echo "refused: judge sweep lookup failed"; continue; }
      [ "$entry" = "$want" ] && { found=1; break; }
    done
    [ -n "$found" ] || echo "A legacy/$path: no sweep commit in the lock has $path with this content; legacy/ admits only byte-for-byte moves"
  done || echo "refused: judge diff/retention pipeline failed"
}

chain() { # LOCK FROZEN CUR RETENTION HEAD-COMMIT
  local lock="$1" previous="$2" current="$3" retention="$4" head="$5"
  local rows root kind source destination detail index=0
  if ! rows="$(lock_rows "$lock")"; then
    printf '%s\n' "$rows"
    return 0
  fi
  root="$(git rev-parse "$head^{tree}")" || { echo "refused: chain HEAD tree lookup failed"; return; }
  while read -r kind source destination || [ -n "$kind" ]; do
    case "$kind" in sweep) ;; *) continue ;; esac
    index=$((index+1))
    if git rev-parse -q --verify "$source^{commit}" >/dev/null \
       && git merge-base --is-ancestor "$source" "$head" \
       && [ "$(git cat-file -t "$destination" 2>/dev/null)" = tree ]; then
      detail="$(judge "$previous" "$destination" "$retention" "$root" "$source" 2>&1)"
      if [ -n "$detail" ]; then
        printf 'sweep %s (%s):\n%s\n' "$index" "${source:0:10}" "$detail"
      fi
      previous="$destination"
    else
      printf "sweep line %s: needs a commit of HEAD's history and a tree: %s %s\n" "$index" "$source" "$destination"
    fi
  done <<<"$rows"
  detail="$(judge "$previous" "$current" "$retention" "$root" 2>&1)"
  if [ -n "$detail" ]; then printf 'since the last sweep:\n%s\n' "$detail"; fi
}

# trusted_base HEAD-COMMIT: the newest commit whose history is trusted (see 5 above).
trusted_base() {
  local ref="${LEGACY_TRUSTED_REF:-origin/main}" rc
  git rev-parse -q --verify "$ref^{commit}" >/dev/null; rc=$?
  if [ "$rc" = 0 ]; then
    git merge-base "$1" "$ref" || echo "refused: trusted_base merge-base failed"
  elif [ "$rc" = 1 ]; then
    git rev-parse -q --verify "$1^{commit}" || echo "refused: trusted_base HEAD lookup failed"
  else
    echo "refused: trusted_base ref lookup failed"
  fi
}

# lock_rows LOCK-FILE: the lock's rows, from its one parser (lib/legacy_lock.py); exit 1 with one
# diagnostic per problem. No code here reads the lock's bytes itself.
lock_rows() {
  checked lock_rows python3 -I "$LOCKPY" rows "$1"
}
# lock_history LOCK-FILE LOCK-PATH TRUSTED: every row of every version of LOCK-PATH in TRUSTED's
# first-parent history is still a row of LOCK-FILE; a malformed trusted version is refused. Prints
# one refusal per problem, naming the newest trusted commit that has a missing row.
lock_history() {
  checked lock_history python3 -I "$LOCKPY" history "$1" "$2" "$3"
}

# history_identity PREFIX HEAD-COMMIT TRUSTED: every version PREFIX/<path> had in TRUSTED's history is
# the version it has at HEAD-COMMIT, for each path HEAD-COMMIT still has. Prints one refusal per path.
history_identity() {
  local prefix="$1" head="$2" trusted="$3"
  checked history_identity python3 -I - "$prefix" "$head" "$trusted" <<'PY'
import subprocess, sys
prefix = sys.argv[1] + "/"
def tokens(args):
    return subprocess.check_output(["git", *args]).split(b"\0")
seen = {}
t = tokens(["log", "--first-parent", "--diff-merges=first-parent", "-z", "--raw", "--no-renames", "--no-abbrev", "--format=", sys.argv[3], "--", prefix]); i = 0
while i < len(t):
    tok = t[i].lstrip(b"\n")
    if tok.startswith(b":") and i + 1 < len(t):
        _, newmode, _, newsha, st = tok[1:].decode().split(" ")
        path = t[i + 1].decode("utf-8", "surrogateescape")
        if not st.startswith("D"):
            seen.setdefault(path, set()).add((newmode, newsha))
        i += 2
    else:
        i += 1
for ent in tokens(["ls-tree", "-r", "-z", "--full-tree", sys.argv[2], "--", prefix]):
    if not ent:
        continue
    meta, path = ent.split(b"\t", 1)
    mode, _, sha = meta.decode().split(" ")
    path = path.decode("utf-8", "surrogateescape")
    other = sorted(v for v in seen.get(path, ()) if v != (mode, sha))
    if other:
        print(f"{path}: was {other[0][1][:10]} (mode {other[0][0]}) in trusted history and is {sha[:10]} (mode {mode}) now: a frozen or swept file is never rewritten, by an edit or by deletion and re-addition")
PY
}

read_loop_scan() {
  printf '%s\n' "$1" | grep -vF '|| [ -n "$' | grep -vF 'grep -nE'
  local statuses=("${PIPESTATUS[@]}") rc
  for rc in "${statuses[@]}"; do [ "$rc" -le 1 ] || { echo "refused: read-loop scan failed"; break; }; done
}
named_readers() {
  # Scan tracked active tooling; the single exclusion expression permits only
  # comments, the parser implementation, and this check declaring its lock path.
  git ls-files -z -- scripts .githooks .github \
    | xargs -0 -r grep -nIE 'legacy[.]lock' -- 2>/dev/null \
    | grep -vE '^[^:]+:[0-9]+:[[:space:]]*#|^scripts/checks/lib/legacy_lock[.]py:|^scripts/checks/legacy_frozen[.]sh:[0-9]+:LOCK=scripts/checks/legacy[.]lock$'
  local outcomes=("${PIPESTATUS[@]}") index
  if [ "${outcomes[0]}" -ne 0 ]; then echo "refused: reader file listing failed"; fi
  for index in 1 2; do
    if [ "${outcomes[index]}" -gt 1 ]; then echo "refused: reader scan failed"; break; fi
  done
}

lock_uses() {
  grep -nE '"\$(LOCK|lock)"' "$SELF" | grep -vE '^[0-9]+:[[:space:]]*#' \
  | grep -vE '(lock_rows|lock_history|chain) "\$(LOCK|lock)"|\[ -f "\$LOCK" \]|echo "=== |no "\$LOCK |local lock="\$1"'
  local statuses=("${PIPESTATUS[@]}") rc
  for rc in "${statuses[@]}"; do [ "$rc" -le 1 ] || { echo "refused: lock-use scan failed"; break; }; done
}

echo "=== negative controls ==="
# The controls build a throwaway repository: none of git's repository variables may reach it (ISS-0015).
source "$ROOT_DIR/scripts/checks/lib/git_sandbox.sh" || exit 2
git_sandbox || exit 2
TMP="$(mktemp -d)" || exit 2
trap 'rm -rf "$TMP"' EXIT
(
  set -e
  cd "$TMP"; git init -q c; cd c
  mkdir -p a b; echo 1 > a/x; echo 2 > b/y; echo 3 > top
  git add -A; git -c user.name=t -c user.email=t@t commit -qm frozen
) || { echo "FATAL: control repository"; exit 2; }
cd "$TMP/c"
# The sweep controls make commits: give the control repository its own identity, so they do not
# depend on the host's git config (a CI runner has none).
git config user.name t; git config user.email t@t
F="$(git rev-parse HEAD^{tree})"
tree_with() { # shell snippet applied to a copy of the frozen tree; prints the new tree
  git read-tree "$F"; git checkout-index -a -f; git clean -fdq; eval "$1"; git add -A >/dev/null; git write-tree; }
printf 'path\tstatus\nb\topen\n' > "$TMP/r_open.tsv"
printf 'path\tstatus\nb\tresolved\n' > "$TMP/r_ok.tsv"
printf 'path\tkind\nb\tx\n' > "$TMP/r_nostatus.tsv"
[ -z "$(judge "$F" "$F" "$TMP/r_open.tsv" "$F")" ] && ok "control: an unchanged tree passes" || no "control: unchanged tree"
T="$(tree_with 'echo new > a/z')"
[ -n "$(judge "$F" "$T" "$TMP/r_ok.tsv" "$F")" ] && ok "control: an added file is refused" || no "control: addition accepted"
T="$(tree_with 'echo changed > a/x')"
[ -n "$(judge "$F" "$T" "$TMP/r_ok.tsv" "$F")" ] && ok "control: a modified file is refused" || no "control: modification accepted"
T="$(tree_with 'rm -rf b')"
[ -n "$(judge "$F" "$T" "$TMP/r_open.tsv" "$F")" ] && ok "control: a deletion under an open row is refused" || no "control: open-row deletion accepted"
[ -n "$(judge "$F" "$T" "$TMP/r_nostatus.tsv" "$F")" ] && ok "control: a deletion without a status column is refused" || no "control: status-less deletion accepted"
[ -z "$(judge "$F" "$T" "$TMP/r_ok.tsv" "$F")" ] && ok "control: a deletion under a resolved row passes" || no "control: resolved-row deletion refused"
printf 'path\tstatus\nb\tresolved\nb/y\topen\n' > "$TMP/r_child.tsv"
T="$(tree_with 'rm -rf b')"
[ -n "$(judge "$F" "$T" "$TMP/r_child.tsv" "$F")" ] && ok "control: a resolved parent does not release a child with an open row" || no "control: open child released by resolved parent"
T="$(tree_with 'rm -f a/x')"
[ -n "$(judge "$F" "$T" "$TMP/r_ok.tsv" "$F")" ] && ok "control: a deletion outside the resolved row is refused" || no "control: unrelated deletion accepted"
# Sweep controls. S is a root commit holding p/moved (and an a/x unlike the frozen one); a sweep
# moves p/moved into legacy/ byte for byte. R0 is a root tree without p/moved, RS one that still has it.
S="$(git commit-tree "$(tree_with 'mkdir -p p; echo m > p/moved; echo changed > a/x')" -m sweep-source)"
R0="$(tree_with 'rm -rf a b top; echo r > rootfile')"
[ -n "$S" ] && [ -n "$R0" ] || { echo "FATAL: sweep control commit"; exit 2; }
RS="$(git rev-parse "$S^{tree}")"
T="$(tree_with 'mkdir -p p; echo m > p/moved')"
[ -z "$(judge "$F" "$T" "$TMP/r_ok.tsv" "$R0" "$S")" ] && ok "control: a byte-for-byte move from a sweep commit, gone from the root, passes" || no "control: a valid sweep was refused: $(judge "$F" "$T" "$TMP/r_ok.tsv" "$R0" "$S")"
[ -n "$(judge "$F" "$T" "$TMP/r_ok.tsv" "$R0")" ] && ok "control: the same addition with no sweep line is refused" || no "control: sweep-less addition accepted"
[ -n "$(judge "$F" "$T" "$TMP/r_ok.tsv" "$RS" "$S")" ] && ok "control: a swept copy that still exists at the root is refused" || no "control: copy kept at the root accepted"
T="$(tree_with 'mkdir -p p; echo m2 > p/moved')"
[ -n "$(judge "$F" "$T" "$TMP/r_ok.tsv" "$R0" "$S")" ] && ok "control: a modified copy of a swept file is refused" || no "control: modified swept copy accepted"
T="$(tree_with 'mkdir -p p; echo m > p/moved; chmod +x p/moved')"
[ -n "$(judge "$F" "$T" "$TMP/r_ok.tsv" "$R0" "$S")" ] && ok "control: a swept copy with another file mode is refused" || no "control: mode change accepted"
T="$(tree_with 'mkdir -p p; echo n > p/new')"
[ -n "$(judge "$F" "$T" "$TMP/r_ok.tsv" "$R0" "$S")" ] && ok "control: a new file with no root original is refused" || no "control: unoriginated addition accepted"
T="$(tree_with 'echo changed > a/x')"
[ -n "$(judge "$F" "$T" "$TMP/r_ok.tsv" "$R0" "$S")" ] && ok "control: an edit of an existing legacy/ file is refused, even matching a sweep commit" || no "control: edit of a frozen file accepted via a sweep"
# After a sweep, the sweep tree is the baseline: deleting a swept file needs a resolved row.
SW1="$(tree_with 'mkdir -p p; echo m > p/moved')"
printf 'path\tstatus\np\topen\n' > "$TMP/r_popen.tsv"; printf 'path\tstatus\np\tresolved\n' > "$TMP/r_pok.tsv"
[ -n "$(judge "$SW1" "$F" "$TMP/r_popen.tsv" "$R0")" ] && ok "control: deleting a swept file under an open row is refused" || no "control: swept file deleted under an open row"
[ -z "$(judge "$SW1" "$F" "$TMP/r_pok.tsv" "$R0")" ] && ok "control: deleting a swept file under a resolved row passes" || no "control: resolved deletion of a swept file refused"
[ -n "$(judge "$SW1" "$(tree_with 'mkdir -p p; echo m > p/moved; echo q > p/other')" "$TMP/r_ok.tsv" "$R0")" ] && ok "control: an addition after the last sweep, with no new sweep line, is refused" || no "control: post-sweep addition accepted"
# Lock-chain controls: H is a commit with S in its history; X is a commit outside it.
H="$(git commit-tree "$R0" -p "$S" -m head)"
# X has the would-be moved file (tree RS), so only its ancestry can refuse it.
X="$(git commit-tree "$RS" -m unrelated)"
[ -n "$H" ] && [ -n "$X" ] || { echo "FATAL: chain control commits"; exit 2; }
# A control lock: its base and tree rows, then the sweep rows given.
HDR="base $S"$'\n'"tree $F"
printf '%s\nsweep %s %s\n' "$HDR" "$S" "$SW1" > "$TMP/l_ok"
[ -z "$(chain "$TMP/l_ok" "$F" "$SW1" "$TMP/r_ok.tsv" "$H")" ] && ok "control: a lock chaining the frozen tree through a sweep to HEAD:legacy passes" || no "control: valid chain refused: $(chain "$TMP/l_ok" "$F" "$SW1" "$TMP/r_ok.tsv" "$H")"
printf '%s\nsweep %s %s\n' "$HDR" "$X" "$SW1" > "$TMP/l_x"
o="$(chain "$TMP/l_x" "$F" "$SW1" "$TMP/r_ok.tsv" "$H")"
# The refused row is skipped, so its addition is then refused too, as an unoriginated one.
[ "$o" = "sweep line 1: needs a commit of HEAD's history and a tree: $X $SW1"$'\n'"since the last sweep:"$'\n'"A legacy/p/moved: no sweep commit in the lock has p/moved with this content; legacy/ admits only byte-for-byte moves" ] && ok "control: a sweep commit outside HEAD's history is refused, as such" || no "control: non-ancestor sweep: '$o'"
[ -n "$(chain "$TMP/l_ok" "$F" "$F" "$TMP/r_popen.tsv" "$H")" ] && ok "control: deleting a swept file after its sweep, under an open row, is refused" || no "control: post-sweep deletion accepted"
# The lock's one parser (lib/legacy_lock.py). Each malformed lock is refused by chain() with the
# parser's exact diagnostic, before any sweep is judged: no byte is a row to one reader and not to another.
ROW="is not a lock row: line 3 must be 'sweep <commit> <tree>', one space between fields, each oid 40 or 64 lowercase hex digits"
refused_lock() { # NAME PRINTF-FORMAT EXPECTED [PRINTF-ARG...]
  local fixture="$TMP/$1" format="$2" expected="$3" actual parsed
  shift 3
  printf "$format" "$@" > "$fixture"
  parsed="$(lock_rows "$fixture")"
  actual="$(chain "$fixture" "$F" "$F" "$TMP/r_popen.tsv" "$H")"
  if [ "$actual" = "$expected" ] && [ "$parsed" = "$expected" ]; then
    ok "control: ${fixture##*/} is refused by the lock parser, as such"
  else
    no "control: ${fixture##*/}: '$actual'"
  fi
}

# A landed sweep row with a NUL in front: GNU grep -x matches the row behind the NUL, sed skips it.
refused_lock "a NUL-prefixed sweep row" '%s\n\0sweep %s %s\n' "$TMP/a NUL-prefixed sweep row:3: holds a NUL byte: the lock is text" "$HDR" "$S" "$SW1"
refused_lock "a CR line ending" '%s\nsweep %s %s\r\n' "$TMP/a CR line ending:3: holds a carriage return: a line ends with a newline (LF) alone" "$HDR" "$S" "$SW1"
refused_lock "a sweep row without its tree" '%s\nsweep %s\n' "$TMP/a sweep row without its tree:3: $ROW" "$HDR" "$S"
refused_lock "a sweep row with a trailing space" '%s\nsweep %s %s \n' "$TMP/a sweep row with a trailing space:3: $ROW" "$HDR" "$S" "$SW1"
refused_lock "a sweep row with a third field" '%s\nsweep %s %s %s\n' "$TMP/a sweep row with a third field:3: $ROW" "$HDR" "$S" "$SW1" "$SW1"
refused_lock "a blank line" '%s\n\nsweep %s %s\n' "$TMP/a blank line:3: $ROW" "$HDR" "$S" "$SW1"
refused_lock "a repeated sweep row" '%s\nsweep %s %s\nsweep %s %s\n' "$TMP/a repeated sweep row:4: repeats line 3: a row is recorded once" "$HDR" "$S" "$SW1" "$S" "$SW1"
refused_lock "a last row without its newline" '%s\nsweep %s %s' "$TMP/a last row without its newline:3: has no newline: every line, the last too, ends with one" "$HDR" "$S" "$SW1"
refused_lock "a lock without its base row" 'tree %s\ntree %s\n' "$TMP/a lock without its base row:1: is not a lock row: line 1 must be 'base <commit>', one space between fields, each oid 40 or 64 lowercase hex digits" "$F" "$F"
refused_lock "a preview lock" '%s\npreview missing: P2\n' "$TMP/a preview lock:3: the move is a --preview build (preview missing: P2); a lock may only record a move generated with its landing prerequisites met (the generator is legacy/scripts/land/legacy_move.sh since the 2026-11-08 sweep)" "$HDR"
refused_lock "a non-ASCII row" '%s\nsweep \377\n' "$TMP/a non-ASCII row:3: holds a non-ASCII byte" "$HDR"
refused_lock "an uppercase oid" '%s\nsweep %s %s\n' "$TMP/an uppercase oid:3: $ROW" "$HDR" "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" "$SW1"
refused_lock "a tab separator" '%s\nsweep\t%s %s\n' "$TMP/a tab separator:3: $ROW" "$HDR" "$S" "$SW1"
S2="$(git commit-tree "$(tree_with 'mkdir -p p; echo m2 > p/moved')" -p "$S" -m sweep-2)"
H2="$(git commit-tree "$R0" -p "$S2" -m head2)"
[ -n "$S2" ] && [ -n "$H2" ] || { echo "FATAL: second sweep control commits"; exit 2; }
SW2="$(tree_with 'mkdir -p p; echo m2 > p/moved')"
printf '%s\nsweep %s %s\nsweep %s %s\n' "$HDR" "$S" "$SW1" "$S2" "$SW2" > "$TMP/l_two"
[ -n "$(chain "$TMP/l_two" "$F" "$SW2" "$TMP/r_ok.tsv" "$H2")" ] && ok "control: a later sweep cannot rewrite a file an earlier sweep moved" || no "control: second sweep rewrote a swept file"
T="$(tree_with 'mkdir -p p; printf m > "p/odd\\name"')"
[ -n "$(judge "$F" "$T" "$TMP/r_ok.tsv" "$R0" "$S")" ] && ok "control: an added path git would C-quote is refused when it has no original" || no "control: C-quoted path accepted"
# A legitimate sweep of p/moved plus a file at ":(literal)p/moved": read as pathspec magic, that path
# would resolve to p/moved, match the sweep commit and pass.
T="$(tree_with 'mkdir -p p ":(literal)p"; echo m > p/moved; echo m > ":(literal)p/moved"')"
[ -n "$(judge "$F" "$T" "$TMP/r_ok.tsv" "$R0" "$S")" ] && ok "control: an added path spelled as pathspec magic is refused" || no "control: pathspec-magic path accepted"
# Trusted-history controls: real commits with legacy/ and the lock. M1 freezes legacy/ = F; M2 sweeps
# p/moved (m) into it. Both are trusted (main's). The candidate commits after them are not.
commit_of() { # parent tree-of-legacy root-file-content lock-text -> commit
  local idx="$TMP/idx.$$" t
  rm -f "$idx"; GIT_INDEX_FILE="$idx" git read-tree --prefix=legacy/ "$2"
  printf '%s' "$3" > "$TMP/rootf"; printf '%s\n' "$4" > "$TMP/lockf"
  GIT_INDEX_FILE="$idx" git update-index --add --cacheinfo "100644,$(git hash-object -w "$TMP/rootf"),rootfile" \
    --cacheinfo "100644,$(git hash-object -w "$TMP/lockf"),L"
  t="$(GIT_INDEX_FILE="$idx" git write-tree)"; rm -f "$idx"
  if [ -n "$1" ]; then git commit-tree "$t" -p "$1" -m c; else git commit-tree "$t" -m c; fi
}
LK1="$(printf 'base %s\ntree %s\n' "$S" "$F")"
M1="$(commit_of "" "$F" r "$LK1")"
M2="$(commit_of "$M1" "$SW1" r "$LK1"$'\n'"sweep $S $SW1")"
[ -n "$M1" ] && [ -n "$M2" ] || { echo "FATAL: trusted-history control commits"; exit 2; }
git show "$M2:L" > "$TMP/lock_same"
[ -z "$(lock_history "$TMP/lock_same" L "$M2")" ] && ok "control: a lock that keeps every trusted line passes" || no "control: unchanged lock refused: $(lock_history "$TMP/lock_same" L "$M2")"
printf '%s\nsweep %s %s\n' "$LK1" "$S2" "$SW2" > "$TMP/lock_rewritten"   # the M2 row replaced
o="$(lock_history "$TMP/lock_rewritten" L "$M2")"
[ "$o" = "lock: 'sweep $S $SW1' (in ${M2:0:10}, trusted history) is not in the lock: recorded lines are only ever added, never rewritten or removed" ] \
  && ok "control: rewriting a trusted sweep line is refused, naming the line" || no "control: rewritten sweep line: '$o'"
printf '%s\n' "$LK1" > "$TMP/lock_dropped"
o="$(lock_history "$TMP/lock_dropped" L "$M2")"
[ "$o" = "lock: 'sweep $S $SW1' (in ${M2:0:10}, trusted history) is not in the lock: recorded lines are only ever added, never rewritten or removed" ] \
  && ok "control: removing a trusted sweep line is refused, naming the line" || no "control: removed sweep line: '$o'"
printf 'base %s\ntree %s\nsweep %s %s\n' "$S2" "$F" "$S" "$SW1" > "$TMP/lock_rebased"
o="$(lock_history "$TMP/lock_rebased" L "$M2")"
[ "$o" = "lock: 'base $S' (in ${M2:0:10}, trusted history) is not in the lock: recorded lines are only ever added, never rewritten or removed" ] \
  && ok "control: changing the trusted base line is refused, naming the line" || no "control: changed base line: '$o'"
# The NUL attack on history: the landed row kept behind a NUL. The parser refuses the lock itself,
# so no row of it can match a trusted row.
printf '%s\n\0sweep %s %s\n' "$LK1" "$S" "$SW1" > "$TMP/lock_nul"
o="$(lock_history "$TMP/lock_nul" L "$M2")"
[ "$o" = "$TMP/lock_nul:3: holds a NUL byte: the lock is text" ] \
  && ok "control: a trusted sweep row kept behind a NUL byte is refused by trusted history" || no "control: NUL-prefixed trusted row: '$o'"
# A malformed version in trusted history is refused, not skipped.
MB="$(commit_of "$M2" "$SW1" r "$LK1"$'\n'"sweep $S $SW1"$'\r')"
o="$(lock_history "$TMP/lock_same" L "$MB")"
[ "$o" = "lock: a trusted version is malformed: ${MB:0:10}:L:3: holds a carriage return: a line ends with a newline (LF) alone" ] \
  && ok "control: a malformed lock in trusted history is refused, not skipped" || no "control: malformed trusted lock: '$o'"
[ -z "$(history_identity legacy "$M2" "$M2")" ] && ok "control: a legacy/ that kept every trusted version passes" || no "control: unchanged history refused: $(history_identity legacy "$M2" "$M2")"
# The attacks. (1) the row rewrite: the candidate replaces M2's row and p/moved's bytes together.
C1="$(commit_of "$M2" "$SW2" r "$LK1"$'\n'"sweep $S2 $SW2")"
o="$(history_identity legacy "$C1" "$M2")"
[ "$o" = "legacy/p/moved: was $(git rev-parse "$SW1:p/moved" | cut -c1-10) (mode 100644) in trusted history and is $(git rev-parse "$SW2:p/moved" | cut -c1-10) (mode 100644) now: a frozen or swept file is never rewritten, by an edit or by deletion and re-addition" ] \
  && ok "control: a swept file rewritten with its lock row is refused by trusted history" || no "control: row-rewrite attack: '$o'"
# (2) delete then re-add: a trusted commit deletes p/moved (a resolved deletion); the candidate re-adds
# it with other bytes under a new sweep line. Each chain step passes; history refuses.
M3="$(commit_of "$M2" "$F" r "$LK1"$'\n'"sweep $S $SW1")"
C2="$(commit_of "$M3" "$SW2" r "$LK1"$'\n'"sweep $S $SW1"$'\n'"sweep $S2 $SW2")"
o="$(history_identity legacy "$C2" "$M3")"
case "$o" in "legacy/p/moved: was $(git rev-parse "$SW1:p/moved" | cut -c1-10) (mode 100644) in trusted history and is $(git rev-parse "$SW2:p/moved" | cut -c1-10) (mode 100644) now: "*)
  ok "control: a frozen file deleted and re-added with other bytes is refused by trusted history" ;; *) no "control: delete-then-re-add attack: '$o'" ;; esac
# The same re-addition with the same bytes is no rewrite.
C3="$(commit_of "$M3" "$SW1" r "$LK1"$'\n'"sweep $S $SW1")"
[ -z "$(history_identity legacy "$C3" "$M3")" ] && ok "control: re-adding a file with its trusted bytes passes" || no "control: identical re-addition refused"
# A branch that rewrote its own lock line and corrected it before landing: its commits are the merge's
# second parent, so the corrected line is no trusted line and the landed lock passes.
B1="$(commit_of "$M2" "$SW1" r "$LK1"$'\n'"sweep $S2 $SW2")"
B2="$(commit_of "$B1" "$SW1" r "$LK1"$'\n'"sweep $S $SW1")"
MM="$(git commit-tree "$(git rev-parse "$B2^{tree}")" -p "$M2" -p "$B2" -m landed)"
[ -n "$MM" ] || { echo "FATAL: merge control commit"; exit 2; }
[ -z "$(lock_history "$TMP/lock_same" L "$MM")" ] && [ -z "$(history_identity legacy "$MM" "$MM")" ] \
  && ok "control: a member branch's corrected intermediate lock is not trusted history (first parents only)" || no "control: second-parent history trusted: $(lock_history "$TMP/lock_same" L "$MM")"
# trusted_base: the merge base with the trusted ref; HEAD itself when the ref is absent.
[ "$(LEGACY_TRUSTED_REF="$M2" trusted_base "$C1")" = "$M2" ] && [ "$(LEGACY_TRUSTED_REF=refs/heads/no-such trusted_base "$C1")" = "$C1" ] \
  && ok "control: the trusted base is the merge base with the trusted ref, else HEAD (everything trusted)" || no "control: trusted_base"
# Exact failure controls: command stubs affect only the subshell invoking the helper.
expect_refusal() {
  local name="$1" want="$2" got; shift 2
  got="$("$@" 2>/dev/null)"
  [ "$got" = "$want" ] && ok "control: $name" || no "control: $name: '$got' (wanted '$want')"
}
env_chain_stub() { local LOCKPY="$1"; chain "$TMP/l_ok" "$F" "$F" "$TMP/r_ok.tsv" "$H"; }
env_history_stub() { local LOCKPY="$1"; lock_history "$TMP/lock_dropped" L "$M1"; }
printf 'base %s\n' "$S" > "$TMP/base_only"
expect_refusal "base-only chain" "$TMP/base_only:2: missing: line 2 must be 'tree <tree>'" chain "$TMP/base_only" "$F" "$F" "$TMP/r_ok.tsv" "$H"
MB0="$(commit_of "" "$F" r "base $S")"
expect_refusal "base-only trusted lock" "lock: a trusted version is malformed: ${MB0:0:10}:L:2: missing: line 2 must be 'tree <tree>'" lock_history "$TMP/lock_dropped" L "$MB0"
for rc in 1 2; do
  printf 'import sys\nprint("stub error", file=sys.stderr)\nsys.exit(%s)\n' "$rc" > "$TMP/parser_stub.py"
  expect_refusal "silent parser exit $rc chain" "refused: lock_rows failed (exit $rc)" env_chain_stub "$TMP/parser_stub.py"
  expect_refusal "silent parser exit $rc history" "refused: lock_history failed (exit $rc)" env_history_stub "$TMP/parser_stub.py"
done
expect_refusal "chain HEAD lookup failure" "refused: chain HEAD tree lookup failed" chain "$TMP/l_ok" "$F" "$F" "$TMP/r_ok.tsv" no-such-head
expect_refusal "judge diff failure" "refused: judge diff/retention pipeline failed" judge no-such-tree "$F" "$TMP/r_ok.tsv" "$F"
expect_refusal "judge retention failure" "refused: judge diff/retention pipeline failed" judge "$SW1" "$F" "$TMP/no-retention" "$F"
expect_refusal "judge root lookup failure" "refused: judge root lookup failed" judge "$F" "$SW1" "$TMP/r_ok.tsv" no-such-tree "$S"
expect_refusal "judge sweep lookup failure" "refused: judge sweep lookup failed"$'\n'"A legacy/p/moved: no sweep commit in the lock has p/moved with this content; legacy/ admits only byte-for-byte moves" judge "$F" "$SW1" "$TMP/r_ok.tsv" "$R0" no-such-commit
expect_refusal "identity log failure" "refused: history_identity failed (exit 1)" history_identity legacy "$M1" no-such-commit
expect_refusal "identity tree failure" "refused: history_identity failed (exit 1)" history_identity legacy no-such-commit "$M1"
expect_refusal "trusted merge-base failure" "refused: trusted_base merge-base failed" env LEGACY_TRUSTED_REF="$M1" bash -c "$(declare -f trusted_base); trusted_base no-such-head"
expect_refusal "trusted HEAD failure" "refused: trusted_base HEAD lookup failed" env LEGACY_TRUSTED_REF=refs/heads/no-such bash -c "$(declare -f trusted_base); trusted_base no-such-head"
# Selectively fail one command while letting all other Git/Python calls run normally.
fail_command() (
  local target="$1"; shift
  git() { if [ "$1" = "$target" ]; then return 2; else command git "$@"; fi; }
  grep() { if [ "$target" = grep ]; then return 2; else command grep "$@"; fi; }
  python3() { if [ "$target" = python3 ]; then return 2; else command python3 "$@"; fi; }
  "$@"
)
expect_refusal "judge Python failure" "refused: judge diff/retention pipeline failed" fail_command python3 judge "$SW1" "$F" "$TMP/r_ok.tsv" "$F"
expect_refusal "judge added-path lookup failure" "refused: judge added-path lookup failed" fail_command ls-tree judge "$F" "$SW1" "$TMP/r_ok.tsv" "$R0" "$S"
expect_refusal "identity Python failure" "refused: history_identity failed (exit 2)" fail_command python3 history_identity legacy "$M1" "$M1"
expect_refusal "trusted ref lookup failure" "refused: trusted_base ref lookup failed" fail_command rev-parse trusted_base "$M1"
expect_refusal "reader listing failure" "refused: reader file listing failed" fail_command ls-files named_readers
expect_refusal "reader scan failure" "refused: reader scan failed" fail_command grep named_readers
expect_refusal "lock-use scan failure" "refused: lock-use scan failed" fail_command grep lock_uses
expect_refusal "read-loop scan failure" "refused: read-loop scan failed" fail_command grep read_loop_scan 'while read x || [ -n "$x" ]'
# Subprocess Git failures must not be mistaken for absent history or a deleted lock.
mkdir "$TMP/bin"
REAL_GIT="$(command -v git)"
cat > "$TMP/bin/git" <<'STUB'
#!/usr/bin/env bash
if [ "$1" = "$FAIL_GIT_COMMAND" ] || { [ "$FAIL_GIT_COMMAND" = blob-and-tree ] && { [ "$1" = cat-file ] || [ "$1" = ls-tree ]; }; }; then exit 2; fi
exec "$REAL_GIT" "$@"
STUB
chmod +x "$TMP/bin/git"
export REAL_GIT
path_failure() { local PATH="$TMP/bin:$PATH"; export FAIL_GIT_COMMAND="$1"; shift; "$@"; }
expect_refusal "lock history log failure" "lock: cannot list the trusted history of L through ${M1:0:10}" path_failure log lock_history "$TMP/lock_dropped" L "$M1"
expect_refusal "lock history blob failure" "lock: cannot read trusted version ${M1:0:10}:L" path_failure cat-file lock_history "$TMP/lock_dropped" L "$M1"
expect_refusal "lock history tree failure" "lock: cannot read trusted version ${M1:0:10}:L" path_failure blob-and-tree lock_history "$TMP/lock_dropped" L "$M1"
# Every shell read loop here reads a last line that has no newline: `read` fails on it, so a bare
# `while read` silently drops it. (The loops' behaviour is checked above; this keeps a new one honest.)
loops="$(grep -nE '^[^#]*while[^#]*[[:space:]]read[[:space:]]' "$SELF")" || { echo "FATAL: cannot read $SELF"; exit 2; }
bad="$(read_loop_scan "$loops")"
[ "$(printf '%s\n' "$loops" | grep -cF '|| [ -n "$')" -ge 3 ] && [ -z "$bad" ] && ok "control: every read loop in this check reads a last line without a newline" || no "control: a read loop drops an unterminated last line: $bad"
cd "$ROOT_DIR"
# The parser is the lock's one reader. Outside legacy/ (frozen, never run) and comments, the lock's
# path is named only by the parser and by this check's LOCK=, and every use of LOCK here hands it to
# the parser (lock_rows, lock_history) or only tests or names it.
named="$(named_readers)"
uses="$(lock_uses)"
[ -z "$named" ] && [ -z "$uses" ] && [ "$(grep -c '' "$SELF")" -gt 0 ] \
  && ok "control: lib/legacy_lock.py is the lock's one reader: no other code names or reads the lock" \
  || no "control: another reader of the lock: ${named}${uses:+ / in this check: $uses}"

echo "=== legacy/ against $LOCK ==="
[ -f "$LOCK" ] || { no "$LOCK is missing"; echo "legacy_frozen: PASS=$PASS FAIL=$FAIL"; exit 1; }
rows="$(lock_rows "$LOCK")" && lock_ok=1 || lock_ok=""
base="" frozen="" nsw=0
while read -r kind a _ || [ -n "$kind" ]; do
  case "$kind" in base) base="$a" ;; tree) frozen="$a" ;; sweep) nsw=$((nsw+1)) ;; esac
done <<<"$rows"
if [ -z "$lock_ok" ]; then
  no "$LOCK is malformed (lib/legacy_lock.py is its format):"; printf '%s\n' "$rows" | sed 's/^/      /'
elif [ "$(git rev-parse -q --verify "$base^{tree}" 2>/dev/null)" != "$frozen" ]; then
  no "the recorded tree $frozen is not the tree of the recorded base $base (or the base is not in this clone's history)"
else
  ok "the lock parses (base, tree, $nsw sweep row(s), nothing else), and tree ${frozen:0:10} is the tree of base ${base:0:10}"
  cur="$(git rev-parse -q --verify HEAD:legacy 2>/dev/null)"
  if [ -z "$cur" ]; then
    no "HEAD has no legacy/ directory"
  else
    [ -f RETENTION.tsv ] || { no "RETENTION.tsv is missing"; }
    out="$(checked chain chain "$LOCK" "$frozen" "$cur" "${RETENTION:-RETENTION.tsv}" HEAD)"
    tb="$(checked trusted_base trusted_base HEAD)"
    hist="$(checked lock_history lock_history "$LOCK" "$LOCK" "$tb"; checked history_identity history_identity legacy HEAD "$tb")"
    [ -z "$hist" ] && ok "trusted history (through ${tb:0:10}): every lock line kept, no legacy/ file rewritten" \
      || { no "legacy/ or its lock rewrote trusted history (through ${tb:0:10}):"; echo "$hist" | sed 's/^/      /'; }
    [ "$nsw" = 0 ] || [ -n "$out" ] || ok "the lock's $nsw sweep line(s) name commits of HEAD's history and chain to HEAD:legacy"
    if [ -z "$out" ]; then
      [ "$cur" = "$frozen" ] && ok "legacy/ is byte-identical to the frozen tree" \
                            || ok "legacy/ differs from the frozen tree only by byte-for-byte sweeps and deletions RETENTION.tsv allows"
    else
      no "legacy/ changed:"; echo "$out" | sed 's/^/      /'
    fi
  fi
fi
echo "legacy_frozen: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
